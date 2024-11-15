use rand::Rng;
use rand_chacha::ChaChaRng;
use rand::SeedableRng;
use image::{ImageBuffer, Rgb};

#[derive(Debug, Clone)]
pub enum Expr {
    X,
    Y,
    Constant(f64),
    Sin(Box<Expr>),
    Cos(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mix(Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>),
    Exp(Box<Expr>),
    Sqrt(Box<Expr>),
}

impl Expr {
    fn eval(&self, x: f64, y: f64) -> f64 {
        match self {
            Expr::X => x,
            Expr::Y => y,
            Expr::Constant(c) => *c,
            Expr::Sin(e) => e.eval(x, y).sin(),
            Expr::Cos(e) => e.eval(x, y).cos(),
            Expr::Add(e1, e2) => {
                (e1.eval(x, y) + e2.eval(x, y)) / 2.0
            }
            Expr::Mult(e1, e2) => e1.eval(x, y) * e2.eval(x, y),
            Expr::Div(e1, e2) => {
                let denom = e2.eval(x, y);
                if denom.abs() < 1e-6 {
                    0.0
                } else {
                    e1.eval(x, y) / denom
                }
            }
            Expr::Mix(c1, c2, e1, e2) => {
                let t = (c1.eval(x, y) + c2.eval(x, y)) / 2.0;
                e1.eval(x, y) * (1.0 - t) + e2.eval(x, y) * t
            }
            Expr::Exp(e) => e.eval(x, y).exp(),
            Expr::Sqrt(e) => e.eval(x, y).sqrt().max(0.0),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Expr::X => "x".to_string(),
            Expr::Y => "y".to_string(),
            Expr::Constant(c) => format!("{:.6}", c),
            Expr::Sin(e) => format!("sin({})", e.to_string()),
            Expr::Cos(e) => format!("cos({})", e.to_string()),
            Expr::Add(e1, e2) => format!("({} + {})", e1.to_string(), e2.to_string()),
            Expr::Mult(e1, e2) => format!("({} * {})", e1.to_string(), e2.to_string()),
            Expr::Div(e1, e2) => format!("({} / {})", e1.to_string(), e2.to_string()),
            Expr::Mix(c1, c2, e1, e2) => format!("mix({}, {}, {}, {})",
                                                 c1.to_string(), c2.to_string(), e1.to_string(), e2.to_string()),
            Expr::Exp(e) => format!("exp({})", e.to_string()),
            Expr::Sqrt(e) => format!("sqrt({})", e.to_string()),
        }
    }
}

pub struct RandomArt {
    rng: ChaChaRng,
}

impl RandomArt {
    pub fn new(seed: u64) -> Self {
        RandomArt {
            rng: ChaChaRng::seed_from_u64(seed),
        }
    }

    fn generate_expr(&mut self, depth: i32) -> Expr {
        if depth <= 0 || self.rng.gen_bool(0.1) {
            match self.rng.gen_range(0..3) {
                0 => Expr::X,
                1 => Expr::Y,
                _ => Expr::Constant(self.rng.gen_range(-1.0..1.0)),
            }
        } else {
            match self.rng.gen_range(0..9) {
                0 => Expr::Sin(Box::new(self.generate_expr(depth - 1))),
                1 => Expr::Cos(Box::new(self.generate_expr(depth - 1))),
                2 => Expr::Add(
                    Box::new(self.generate_expr(depth - 1)),
                    Box::new(self.generate_expr(depth - 1))
                ),
                3 => Expr::Mult(
                    Box::new(self.generate_expr(depth - 1)),
                    Box::new(self.generate_expr(depth - 1))
                ),
                4 => Expr::Div(
                    Box::new(self.generate_expr(depth - 1)),
                    Box::new(self.generate_expr(depth - 1))
                ),
                5 => Expr::Mix(
                    Box::new(self.generate_expr(depth - 1)),
                    Box::new(self.generate_expr(depth - 1)),
                    Box::new(self.generate_expr(depth - 1)),
                    Box::new(self.generate_expr(depth - 1))
                ),
                6 => Expr::Exp(Box::new(self.generate_expr(depth - 1))),
                7 => Expr::Sqrt(Box::new(self.generate_expr(depth - 1))),
                _ => Expr::Sin(Box::new(self.generate_expr(depth - 1))),
            }
        }
    }

    pub fn generate(&mut self, width: u32, height: u32) -> (ImageBuffer<Rgb<u8>, Vec<u8>>, String) {
        let red = self.generate_expr(16);
        let green = self.generate_expr(16);
        let blue = self.generate_expr(16);

        let mut img = ImageBuffer::new(width, height);

        for y in 0..height {
            for x in 0..width {
                let px = (x as f64 / width as f64) * 2.0 - 1.0;
                let py = (y as f64 / height as f64) * 2.0 - 1.0;

                let r = ((red.eval(px, py) + 1.0) * 127.5) as u8;
                let g = ((green.eval(px, py) + 1.0) * 127.5) as u8;
                let b = ((blue.eval(px, py) + 1.0) * 127.5) as u8;

                img.put_pixel(x, y, Rgb([r, g, b]));
            }
        }

        let expr_text = format!(
            "R: {}\nG: {}\nB: {}",
            red.to_string(),
            green.to_string(),
            blue.to_string()
        );

        (img, expr_text)
    }
}