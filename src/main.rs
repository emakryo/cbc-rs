use std::io::Read;
use structopt::StructOpt;
use cbc_rs::parser::parse_source;
use anyhow::Result;

#[derive(Debug, StructOpt)]
struct Args {
    #[structopt(name = "FILE")]
    file_name: String,
}

#[paw::main]
fn main(args: Args) -> Result<()> {
    println!("{:?}", &args);

    let import_paths = vec!["cbc-1.0/import"];

    let mut file = std::fs::File::open(&args.file_name)?;
    let mut buf = vec![];
    file.read_to_end(&mut buf)?;

    let source = std::str::from_utf8(&buf)?;

    let ast = parse_source(source, &import_paths);

    println!("{:?}", ast);

    Ok(())
}
