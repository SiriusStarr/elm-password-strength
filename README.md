# PasswordStrength

[![Build Status](https://travis-ci.com/SiriusStarr/elm-password-strength.svg?branch=master)](https://travis-ci.com/SiriusStarr/elm-password-strength)

## Introduction

This package provides a number of tools for calculating the "strength" of a
password in order to provide feedback to a user (and guide them in creating a
more secure password).  Information on the various modules follows.

## Modules

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

Cons:

* Somewhat computationally intensive (though still sufficiently performant for almost any
use case)
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

* Somewhat computationally intensive (though still sufficiently performant for almost any
use case)
* Large asset size (requires large dictionaries)
* A few odd design choices.
