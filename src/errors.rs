pub enum Error {
    EmptyQuery,
}

pub fn handle_error(error: Error) {
    let err_msg = match error {
        Error::EmptyQuery => "Query must not be empty for deletion",
    };
    panic!("{err_msg}");
}
