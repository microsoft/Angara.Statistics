### 0.1.5

* Fixed a bug in Gamma random number generator.
* Changed parameterization of Exponential distribution.
* Added logit and logistic functions.
* Added fromPiecewise helper method to build Mixture distribution
* Renamed SamplerResult to SamplerCheckpoint and added burnInTrace to the structure
* Changed signature of continuemcmc

### 0.1.4 - 2016-04-20

* Sampler state can be serialized to continue unfinished computation.
* Fixed Mersenne twister copy constructor

### 0.1.3 - 2016-04-12

* Change layout of the repository to match [ProjectScaffold](http://fsprojects.github.io/ProjectScaffold/) recommendation.
* Fix bisection algorithm of `ridders`.
* Add documentation
* Switch back to .NET Framework 4.5.2

### 0.1.2 - 2016-03-11

* `prior` field of `ParameterDefinition` structure is a function instead of a Distribution.
* Redesign of `Parameters.Add` overloads.
* Switch to .NET Framework 4.6.1

### 0.1.1 - 2016-02-17

Initial NuGet release.
 