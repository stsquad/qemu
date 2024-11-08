// Copyright 2024, Linaro Limited
// Author(s): Manos Pitsidianakis <manos.pitsidianakis@linaro.org>
// SPDX-License-Identifier: GPL-2.0-or-later

use std::{env, path::Path};

use version_check as rustc;

fn main() {
    // Placing bindings.rs.inc in the source directory is supported
    // but not documented or encouraged.
    let path = env::var("MESON_BUILD_ROOT")
        .unwrap_or_else(|_| format!("{}/src", env!("CARGO_MANIFEST_DIR")));

    let file = format!("{}/bindings.rs.inc", path);
    if !Path::new(&file).exists() {
        panic!(concat!(
            "No generated C bindings found! If you want to run `cargo`, start a subshell\n",
            "with `meson devenv`, or point MESON_BUILD_ROOT to the top of the build tree."
        ));
    }

    println!("cargo:rustc-env=BINDINGS_RS_INC={}", file);

    // Check for available rustc features
    if rustc::is_min_version("1.77.0").unwrap_or(false) {
        println!("cargo:rustc-cfg=has_offset_of");
    }

    println!("cargo:rerun-if-changed=build.rs");
}
