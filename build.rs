use std::io::Result;

fn main() -> Result<()> {
    prost_build::compile_protos(&["protos/codegen.proto"], &["protos/"])?;
    Ok(())
}
