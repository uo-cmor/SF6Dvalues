# SF6Dvalues 0.6.0

* SF-12 version 1 scores can now be calculated by `PCS()`, `MCS()`, and similar.

* `SF12()` and related functions now accept factors as input.

* Condition handling now follows `rlang` best practices.

# SF6Dvalues 0.5.1

* Correctly export & document `mapping()`.

# SF6Dvalues 0.5.0

* New `mapping()` calculates mapping from SF-6D to other utility instrument
  value sets.

* New `hk()`, `erum()`, `spain()`, and `portugal()` SF-6D(SF-36) value sets.

# SF6Dvalues 0.4.0

* `SF6D.as_SF6D.SF6Dvalues_SF36()` now correctly calculates SF-6D(SF-36)
  profile.

* New `SF36_scores()` calculates SF-36 domain scale scores.

# SF6Dvalues 0.3.0

* `utility()`, `sf6d_utility()`, and `uk()` now work for SF-36-derived SF-6D
  profiles.

* New `oz()` calculates Australian population utility values (SF-36-derived
  SF-6D only).

* New `SF36` class for SF-36 responses.

* New `SF12_scores()` calculates SF-12 health domain scale scores (50/10
  norm-based).

# SF6Dvalues 0.2.0

* New `PCS()` and `MCS()` calculate SF-12 Component Summary scores.

# SF6Dvalues 0.1.0

* First release: contains `SF12` and `SF6D` classes and `sf6d_utility()` and
  `utility()` to calculate utility values.

* Added a `NEWS.md` file to track changes to the package.
