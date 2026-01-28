fn main() {
    std::process::exit(match aria::cli::run() {
        Ok(()) => 0,
        Err(code) => code,
    });
}
