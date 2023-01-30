#!/bin/bash
config=$1

cmake_options="-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
if [ $config == "debug" ]; then
    cmake_options="${cmake_options} -DCMAKE_BUILD_TYPE=Debug"
else
    cmake_options="${cmake_options} -DCMAKE_BUILD_TYPE=Release"
fi

# Add things to cmake_options as needed in the method shown above
echo "Moving into build/${config}"
mkdir -p build/${config}
pushd build/${config}
echo "cmake ${cmake_options} $(dirs -0)"
cmake ${cmake_options} $(dirs -0) 
popd
echo "Moving build/${config}/compile_commands.json to build/compile_commands.json"
mv build/${config}/compile_commands.json build/compile_commands.json


