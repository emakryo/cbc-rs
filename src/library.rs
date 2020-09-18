use crate::error::Error;
use std::io::Read;
use std::path::Path;

pub fn load<P: AsRef<Path>>(libid: &str, header_paths: &[P]) -> Result<String, Error> {
    let lib_filename = libid.replace(".", "/");

    for load_path in header_paths {
        let mut buf = String::new();
        let mut lib_path = load_path.as_ref().to_owned();
        lib_path.push(&format!("{}.hb", &lib_filename));
        let r = std::fs::File::open(&lib_path);
        let mut f = if let Ok(f) = r {
            f
        } else {
            continue;
        };
        let r = f.read_to_string(&mut buf);
        if let Ok(_) = r {
            return Ok(buf);
        }
    }

    Err(Error::Import(libid.to_string()))
}
