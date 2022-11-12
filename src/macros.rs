#[macro_export]
macro_rules! string_enum {
    (
        $(#[$meta:meta])*
        $name:ident {
            $(
                $(#[$type_meta:meta])*
                $type:ident $(= $repr:expr)?
            ),*
        }
    ) => {
        $(#[$meta])*
        pub enum $name {
            $(
                $(#[$type_meta])*
                $type
            ),+
        }

        impl $name {
            pub fn as_str(&self) -> Option<&'static str> {
                match self {
                    $(Self::$type => crate::string_enum! { @option $($repr)? },)+
                }
            }

            pub fn from_str(s: &str) -> Option<$name> {
                match s {
                    $($($repr => Some(Self::$type),)?)*
                    _ => None,
                }
            }
        }
    };
    (@option $repr:expr) => {
        Some($repr)
    };
    (@option) => {
        None
    };
}
