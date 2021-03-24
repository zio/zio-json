---
id: overview_index
title: "Summary"
---

ZIO JSON

## Installation

This project is cross built and published for @CROSS_VERSIONS@

For latest snapshot use
```scala
resolvers ++= Seq("public", "snapshots", "releases").map(Resolver.sonatypeRepo)
libraryDependencies += "@ORG@" % "@NAME@" % "@SNAPSHOT_VERSION@"
```
or 
```scala
resolvers ++= Seq("public", "snapshots", "releases").map(Resolver.sonatypeRepo)
libraryDependencies += "@ORG@" % "@NAME@" % "latest.snapshot"
```

For latest stable release use
```scala
libraryDependencies += "@ORG@" % "@NAME@" % "@RELEASE_VERSION@"
```
or
```scala
resolvers ++= Seq("public", "snapshots", "releases").map(Resolver.sonatypeRepo)
libraryDependencies += "@ORG@" % "@NAME@" % "latest.release"
```