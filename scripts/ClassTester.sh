#!/bin/sh

pwd=`pwd`

contege="${pwd}/bin"
contegeLibs="${pwd}/lib/scala-lib-2.10.2.jar:${pwd}/lib/asm-tree-4.0.jar:${pwd}/lib/asm-4.0.jar:${pwd}/lib/tools.jar:${pwd}/lib/testSkeleton.jar:${pwd}/lib/commons-io-2.0.1.jar:${pwd}/lib/jpf.jar:${pwd}/lib/bcel-5.2.jar"
contegeOwnLibs="${pwd}/ownLibs/javaModel.jar:${pwd}/ownLibs/clinitRewriter.jar:${pwd}/ownLibs/ConTeGe.jar"

# example: test XYSeries class from jfreechart-0.9.8 --- replace this with your own class under test
bmBase="${pwd}/benchmarks/pldi2012/"
bm="XYSeries"
testedJar="${bmBase}/${bm}/jar/jfreechart-0.9.8_rewritten.jar"
testedJarLibs="${bmBase}/${bm}/lib/jcommon-0.8.0.jar:${bmBase}/${bm}/clinit.jar"
testedJarEnvTypes="${bmBase}/${bm}/env_types.txt"


# ClassTester arguments:
#
# 0:被测试的类
# 0: the class under test (CUT)
# 1:带有辅助类型名称的文件
# 1: file with names of auxiliary types
# 2: random seed
# 3: max。nb试图生成后缀(即它应该运行多长时间)
# 3: max. nb of tries to generate suffixes (i.e. how long it should run)
# 4:结果文件(只有在发现线程安全违规时才写)
# 4: result file (only written when a thread safety violation is found)
# 5:是否在每次测试之前重置静态状态(只有在使用ClinitRewriter检测类时才有效)
# 5: whether to reset static state before each test (only works when classes have been instrumented with ClinitRewriter)

seed=3
maxSuffixGenTries=100

cmd="java -cp ${contegeLibs}:${contege}:${contegeOwnLibs}:${testedJar}:${testedJarLibs} contege.ClassTester org.jfree.data.XYSeries ${testedJarEnvTypes} ${seed} ${maxSuffixGenTries} result.out false"

echo "${cmd}\n"
# 解析并执行
eval ${cmd}
cat result.out
