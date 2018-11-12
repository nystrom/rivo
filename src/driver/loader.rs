use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;

use std::collections::HashMap;
use std::path::PathBuf;
use std::fs;
use std::io;

pub type Input = String;

pub type LoaderResult<A> = io::Result<A>;

#[derive(Clone, Debug)]
pub struct Loader {
    loaded_sources: HashMap<Source, Input>,
    paths: Vec<PathBuf>,
}

impl Loader {
    pub fn new_with_paths(paths: Vec<PathBuf>) -> Loader {
        Loader {
            loaded_sources: HashMap::new(),
            paths: paths,
        }
    }
    pub fn new() -> Loader {
        Loader::new_with_paths(vec![
            PathBuf::from(r"."),
            PathBuf::from(r"./lib"),
        ])
    }

    pub fn locate_bundle(&mut self, name: &Name) -> LoaderResult<Source> {
        match name {
            Name::Id(x) => {
                if let Some(ch) = x.to_string().chars().next() {
                    if ch.is_uppercase() {
                        for dir in &self.paths {
                            if ! dir.is_dir() {
                                continue;
                            }

                            let mut ivo_file = dir.clone();
                            ivo_file.set_file_name(x.to_string());
                            ivo_file.set_extension("ivo");

                            let path = fs::canonicalize(ivo_file)?;
                            if path.exists() && path.is_file() {
                                return Ok(Source::FileSource(path.to_path_buf()));
                            }
                        }
                    }
                }
            },
            _ => {},
        }

        Err(io::Error::new(io::ErrorKind::NotFound, "no file found in source path"))
    }

    pub fn load_source(&mut self, source: &Source) -> LoaderResult<Input> {
        use std::fs::File;
        use std::io::Read;
        use std::io::BufReader;

        // Check the cache.
        match self.loaded_sources.get(source) {
            Some(input) => return Ok(input.clone()),
            _ => {},
        }

        match source {
            Source::NoSource => {
                Err(io::Error::new(io::ErrorKind::NotFound, "no source to load"))
            },
            Source::FileSource(ref file) => {
                // Slurp.
                let file = File::open(file)?;
                let mut buf_reader = BufReader::new(file);
                let mut input = String::new();
                buf_reader.read_to_string(&mut input)?;

                // Update the cache.
                self.loaded_sources.insert(source.clone(), input.clone());

                Ok(input)
            },
            Source::StringSource(ref input) => {
                // Update the cache.
                self.loaded_sources.insert(source.clone(), input.clone());

                Ok(input.clone())
            },
        }
    }
}
