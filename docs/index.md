---
id: index
title: "Getting Started with ZIO Json"
sidebar_label: "Getting Started"
---

## Introduction

The goal of this project is to create the best all-round JSON library for Scala:

- **Performance** to handle more requests per second than the incumbents, i.e. reduced operational costs.
- **Security** to mitigate against adversarial JSON payloads that threaten the capacity of the server.
- **Fast Compilation** no shapeless, no type astronautics.
- **Future-Proof**, prepared for Scala 3 and next generation Java.
- **Simple** small codebase, short and concise documentation that covers everything.
- **Helpful errors** are readable by humans and machines.
- **ZIO Integration** so nothing more is required.

## Installation

For the latest snapshot use:

```scala
resolvers ++= Seq("public", "snapshots", "releases").map(Resolver.sonatypeRepo)
libraryDependencies += "dev.zio" % "zio-json" % "@SNAPSHOT_VERSION@"
```

For the latest stable release use:

```scala
libraryDependencies += "dev.zio" % "zio-json" % "@RELEASE_VERSION@"
```
