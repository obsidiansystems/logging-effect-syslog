# logging-effect-syslog
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/logging-effect-syslog.svg)](https://hackage.haskell.org/package/logging-effect-syslog) [![Github CI](https://github.com/obsidiansystems/logging-effect-syslog/workflows/github-action/badge.svg)](https://github.com/obsidiansystems/logging-effect-syslog/actions) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/logging-effect-syslog/blob/master/LICENSE)

A logging `Handler` for [logging-effect](https://hackage.haskell.org/package/logging-effect) that uses [hsyslog](https://hackage.haskell.org/package/hsyslog) to write log messages to a posix-compliant [syslog](https://pubs.opengroup.org/onlinepubs/9699919799/functions/syslog.html).
