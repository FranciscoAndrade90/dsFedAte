# dsRctFed 0.0.0.9000

* Added `dsBase::checkPermissivePrivacyControlLevel()` call to `localAteDS()`.
* Added disclosure risk check via `dsBase::listDisclosureSettingsDS()` to `localAteDS()`.
* Added explicit numeric parsing validation for `tau_str` and `gamma_str` arguments.
* Declared `cli` and `dsBase` under `Imports` in `DESCRIPTION`.
* Added unit tests for `localAteDS()`, `.checkDisclosureRisk()`, and parsing helpers.
