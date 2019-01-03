#!/bin/bash

if [ "$1" = "CovCon" ]
then
	# 设置工具名称
	export toolName="CovCon"
	# 设置最长时间，一个小时60*60
	export timeout="146"
fi
# 启动脚本
./scripts/testRun.sh



