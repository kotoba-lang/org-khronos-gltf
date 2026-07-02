# kotoba-lang/gltf

Zero-dep portable `.cljc` — restored from the legacy `kami-engine/kami-gltf` Rust crate
(`kotoba-lang/kami-engine`, deleted in PR #82 "Remove Rust workspace from kami-engine")
as part of the **clj-wgsl migration** (ADR-2607010930, `com-junkawasaki/root`).

## What this is

`kami-gltf/src/lib.rs` (161 lines) was a minimal binary glTF 2.0 (`.glb`) *exporter*:
given a `LoadedMesh` (interleaved `[pos3 norm3 uv2]` × N float vertices + u32 indices)
and a flat RGBA base color, it wrote out a self-contained, 4-byte-aligned `.glb` byte
buffer (JSON chunk + BIN chunk), with no external glTF/JSON crate dependency. Despite
the crate name suggesting a *loader*, there was no parsing/loading logic in the file —
this port is faithful to that (there was nothing else to port).

`src/gltf.cljc` ports every function 1:1 as pure CLJC data + functions:

- `u32->le-bytes` / `le-bytes->u32` / `f32->le-bytes` — little-endian byte codecs
  (the only platform-divergent piece, isolated behind `#?(:clj ... :cljs ...)`)
- `compute-bounds` — min/max position bounds over the interleaved vertex buffer
- `build-gltf-json` — the glTF JSON chunk shape (asset/scene/nodes/meshes/materials/
  accessors/bufferViews/buffers), matching the original `serde_json::json!` literal
- `->json` — minimal hand-rolled JSON serializer (no external JSON dependency, mirrors
  the original's zero-dependency stance)
- `export-glb-byte-seq` — pure, portable core: mesh + color -> vector of byte ints
- `export-glb` — wraps the above into the platform's native byte buffer (`byte[]` on
  the JVM, `js/Uint8Array` in ClojureScript)

## Tests

`test/gltf_test.cljc` — 6 tests / 14 assertions, 0 failures:

- `glb-header-valid` — ported 1:1 from the original's `#[test] fn glb_header_valid`
- `namespace-loads` — smoke test
- plus additional coverage for the JSON chunk tag, bounds computation, byte
  round-tripping, and JSON serialization shapes

## Develop

```bash
clojure -M:test
```
