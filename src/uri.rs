use std::{borrow::Cow, hash::Hash, ops::Deref, str::FromStr};

use serde::{Deserialize, Serialize, de::Error};
use std::path::{Path, PathBuf};

/// Newtype struct around `fluent_uri::Uri<String>` with serialization implementations that use `as_str()` and 'from_str()' respectively.
#[derive(Debug, Clone)]
pub struct Uri(fluent_uri::Uri<String>);

impl Serialize for Uri {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.as_str().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Uri {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let string = String::deserialize(deserializer)?;
        fluent_uri::Uri::<String>::parse(string)
            .map(Uri)
            .map_err(|err| Error::custom(err.to_string()))
    }
}

impl Ord for Uri {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl PartialOrd for Uri {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl FromStr for Uri {
    type Err = fluent_uri::error::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // TOUCH-UP:
        // Use upstream `FromStr` implementation if and when
        // https://github.com/yescallop/fluent-uri-rs/pull/10
        // gets merged.
        // fluent_uri::Uri::from_str(s).map(Self)
        fluent_uri::Uri::parse(s).map(|uri| Self(uri.to_owned()))
    }
}

impl Deref for Uri {
    type Target = fluent_uri::Uri<String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/*
    TOUCH-UP: `PartialEq`, `Eq` and `Hash` could all be derived
    if and when the respective implementations get merged upstream:
    https://github.com/yescallop/fluent-uri-rs/pull/9
*/
impl PartialEq for Uri {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for Uri {}

impl Hash for Uri {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

#[cfg(not(windows))]
pub use std::fs::canonicalize as strict_canonicalize;

/// On Windows, rewrites the wide path prefix `\\?\C:` to `C:`  
/// Source: https://stackoverflow.com/a/70970317
#[inline]
#[cfg(windows)]
fn strict_canonicalize<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
    use std::io;

    fn impl_(path: PathBuf) -> std::io::Result<PathBuf> {
        let head = path
            .components()
            .next()
            .ok_or(io::Error::new(io::ErrorKind::Other, "empty path"))?;
        let disk_;
        let head = if let std::path::Component::Prefix(prefix) = head {
            if let std::path::Prefix::VerbatimDisk(disk) = prefix.kind() {
                disk_ = format!("{}:", disk as char);
                Path::new(&disk_).components().next().ok_or(io::Error::new(
                    io::ErrorKind::Other,
                    "failed to parse disk component",
                ))?
            } else {
                head
            }
        } else {
            head
        };
        Ok(std::iter::once(head)
            .chain(path.components().skip(1))
            .collect())
    }

    let canon = std::fs::canonicalize(path)?;
    impl_(canon)
}

impl Uri {
    pub fn to_file_path(&self) -> Option<Cow<Path>> {
        let path = match self.path().decode().into_string_lossy() {
            Cow::Borrowed(ref_) => Cow::Borrowed(Path::new(ref_)),
            Cow::Owned(owned) => Cow::Owned(PathBuf::from(owned)),
        };

        if cfg!(windows) {
            let authority = self.authority().expect("url has no authority component");
            let host = authority.host();
            if host.is_empty() {
                // very high chance this is a `file:///` uri
                // in which case the path will include a leading slash we need to remove
                let host = path.to_string_lossy();
                let host = &host[1..];
                return Some(Cow::Owned(PathBuf::from(host)));
            }

            let host = format!("{host}:");
            Some(Cow::Owned(
                Path::new(&host)
                    .components()
                    .chain(path.components())
                    .collect(),
            ))
        } else {
            Some(path)
        }
    }

    pub fn from_file_path<A: AsRef<Path>>(path: A) -> Option<Self> {
        let path = path.as_ref();

        let fragment = if path.is_absolute() {
            Cow::Borrowed(path)
        } else {
            match strict_canonicalize(path) {
                Ok(path) => Cow::Owned(path),
                Err(_) => return None,
            }
        };

        let raw_uri = if cfg!(windows) {
            // we want to parse a triple-slash path for Windows paths
            // it's a shorthand for `file://localhost/C:/Windows` with the `localhost` omitted
            format!("file:///{}", fragment.to_string_lossy().replace("\\", "/"))
        } else {
            format!("file://{}", fragment.to_string_lossy())
        };

        Uri::from_str(&raw_uri).ok()
    }
}

#[cfg(test)]
mod tests {
    use super::strict_canonicalize;
    use super::Uri;
    use std::path::Path;

    #[test]
    #[cfg(windows)]
    fn test_idempotent_canonicalization() {
        let lhs = strict_canonicalize(Path::new(".")).unwrap();
        let rhs = strict_canonicalize(&lhs).unwrap();
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn test_path_roundtrip_conversion() {
        let src = strict_canonicalize(Path::new(".")).unwrap();
        let conv = Uri::from_file_path(&src).unwrap();
        let roundtrip = conv.to_file_path().unwrap();
        assert_eq!(src, roundtrip, "conv={conv:?}",);
    }

    #[test]
    #[cfg(windows)]
    fn test_windows_uri_roundtrip_conversion() {
        use std::str::FromStr;

        let uri = Uri::from_str("file:///C:/Windows").unwrap();
        let path = uri.to_file_path().unwrap();
        assert_eq!(&path, Path::new("C:/Windows"), "uri={uri:?}");

        let conv = Uri::from_file_path(&path).unwrap();

        assert_eq!(
            uri,
            conv,
            "path={path:?} left={} right={}",
            uri.as_str(),
            conv.as_str()
        );
    }
}
