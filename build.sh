if [ ! -d bin ]; then
  mkdir bin
fi

scalac -d bin/ -cp lib/spray-json_2.11-1.3.2.jar:lib/shapeless_2.11-2.2.3.jar src/main/sort/*.scala src/main/sort/products/*.scala
