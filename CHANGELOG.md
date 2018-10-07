# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.6.1] - 2018-10-07
### Added

- Methods for inspecting the topology of the system processors (#21)
  - `VersionInformation::local_logical_processor_id`
  - `VersionInformation::max_logical_processor_ids`
  - `Master::extended_topology_enumeration`
