Changelog
---------
## 0.10.0
* Update to LTS-10.4
* Drop support for lts 7 and 6. This only means that they are removed from CI.
  However, the constraints for packages besides the stack.yaml file have not been
  changed
* Minor improvements to parsing

## 0.1.1
* `FromJSON` and `TOJSON` instances for the core types
* `stack.yaml` updated to lts-7.9

## 0.1.0
* Added basic interface and types
* Supports the VCF 1.2 specification
* Most of the parsers are Applicative Functors instead of monads
