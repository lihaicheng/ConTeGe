使用说明：
原始的程序实际上没有办法直接运行，经过修改，可以在根目录下运行 ./scripts/measureTimeToBug.sh 
ConTeGe
=======

ConTeGe (short for Concurrent Test Generator) is a framework for generating sequential and concurrent unit tests. It is a research platform to build tools for finding correctness and performance problems in sequential and concurrent Java classes. In particular, ConTeGe is the basis for the following approaches:

### An automatic and precise thread safety checker that found previously unknown bugs in the JDK
 
See [thread-safe.org](http://thread-safe.org) for details.

[*Fully Automatic and Precise Detection of Thread Safety Violations*](http://mp.binaervarianz.de/pldi2012.pdf)  
by Michael Pradel and Thomas R. Gross  
at Conference on Programming Language Design and Implementation (PLDI), 2012

### An approach for automatic substitutability testing that finds various bugs in widely used Java classes

See [this page](http://mp.binaervarianz.de/icse2013/) for details.

[*Automatic Testing of Sequential and Concurrent Substitutability*](http://mp.binaervarianz.de/icse2013.pdf)  
by Michael Pradel and Thomas R. Gross  
at International Conference on Software Engineering (ICSE), 2013

### *SpeedGun*, a performance regression testing tool for thread-safe classes

See the [SpeedGun branch](https://github.com/michaelpradel/ConTeGe/tree/SpeedGun) for details.

[*Performance Regression Testing of Concurrent Classes*](http://mp.binaervarianz.de/issta2014.pdf)  
by Michael Pradel, Markus Huggler, and Thomas R. Gross  
at International Symposium on Software Testing and Analysis (ISSTA), 2014



