# PasswordStrength

![Build Status](https://github.com/SiriusStarr/elm-password-strength/workflows/Tests/badge.svg)

## Introduction

This package provides a number of tools for calculating the "strength" of a
password in order to provide feedback to a user (and guide them in creating a
more secure password).  Information on the various modules follows.

## Modules

### Rumkin

`PaswordStrength.Rumkin` is a native Elm implmentation of the popular password
strength checker available at
[rumkin.com](http://rumkin.com/tools/password/passchk.php).  While less thorough
than the zxcvbn modules, it is considerably lighter, as its common password
dictionary is much smaller and unordered.

Pros:

* Light asset size
* Some checking of common passwords
* Considers frequency of character sequences (e.g. "qu" is more likely than
  "qd") to be "smarter" about entropy than merely the sequence space.

Cons:

* Common password check does not factor into strength determination, only
  warnings.
* No support for repeats (e.g. "aaaaaaaaaaaaaaaaaaaa" is considered "Strong").
* No support for dates (e.g. "JohnDoe02/13/1977" is considered "Strong").
* No support for sequences (e.g. "abcdefghijklmnopqrstuvwxyz" is considered
  "Strong").
* No support for key adjacency (e.g. "qazxswedcvfrtgbnhy" is considered
  "Strong").
* No support for common words (that are not common passwords), e.g. "Ascension"
  is considered "Reasonable".

### ZxcvbnPlus

`PasswordStrength.ZxcvbnPlus` is a native Elm implementation of the popular
zxcvbn password strength javascript library, with some tweaks and fixes.
`Zxcvbn` is available if you require exactly identical results to the reference
implementation, however. More information on zxcvbn may be found on its
[GitHub](https://github.com/dropbox/zxcvbn).

Pros:

* Functions similarly to actual password crackers
* Provides a reasonable estimate of the actual security of a password
* Encourages choosing an actually secure password, rather than one that simply
  conforms to arbitrary "must include at least 1 x and 2 y" rules that result in
  insecure passwords like "P@ssw0rd".
* More "Elm"-y (prefers parsers to regexes, uses custom types, etc.).

Cons:

* Somewhat computationally intensive (though still sufficiently performant for
  almost any use case)
* Large asset size (requires large dictionaries)

### Zxcvbn

`PasswordStrength.Zxcvbn` is a native Elm implementation of the popular zxcvbn
password strength javascript library. You **likely want to use `ZxcvbnPlus`
instead**, unless you require exactly identical results to the reference
implementation. More information on zxcvbn may be found on its
[GitHub](https://github.com/dropbox/zxcvbn).

Pros:

* Functions similarly to actual password crackers
* Provides a reasonable estimate of the actual security of a password
* Encourages choosing an actually secure password, rather than one that simply
  conforms to arbitrary "must include at least 1 x and 2 y" rules that result in
  insecure passwords like "P@ssw0rd".

Cons:

* Somewhat computationally intensive (though still sufficiently performant for
  almost any use case)
* Large asset size (requires large dictionaries)
* A few odd design choices (e.g. matches are unnecessarily sorted in
  intermediate steps).

## Changelog

### 1.0.2

* Update reference year to 2021
* Update recent year regex for `Zxcvbn` to include 2021
* Add tests for 2021 as a recent year
* Switch to Github CI

### 1.0.1

* Fix Elm 0.19.1 incompatibility caused by 0.19.1 choking on long lines in
  `Internal.Zxcvbn.FrequencyLists.elm`
* Update reference year to 2020
* Update recent year regex for `Zxcvbn` to include 2020; this technically brings
  `Zxcvbn` out of conformance with the reference implementation
* Add tests for 2020 as a recent year

### 1.0.0

* Initial release
