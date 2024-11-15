fn main() {
    let seed = 14599; // Example seed value
    let mut random_art = random_art::RandomArt::new(seed);
    let (img, expressions) = random_art.generate(600, 600);
    
    println!("Random Art generated successfully!");
    println!("Expressions:");
    println!("{}", expressions);

    img.save("random_art.png").unwrap();
}