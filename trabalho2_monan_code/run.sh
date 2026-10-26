#!/bin/bash

# Script de Execução do Trabalho 2

SRC_FILE="stability_indices_standalone.F90"
OUT_DIR="resultados"

mkdir -p "$OUT_DIR"

if [ ! -f "$SRC_FILE" ]; then
    echo "ERRO: Arquivo $SRC_FILE nao encontrado!"
    exit 1
fi
echo ""
echo " 1. Registrando Metadados do Hardware e Ferramentas "
echo "----------------------------------------------------"
{
    echo "=== CPU E ARQUITETURA ==="
    lscpu
    echo ""
    echo "=== MEMORIA ==="
    free -h
    echo ""
    echo "=== COMPILADOR ==="
    gfortran --version | head -n 1
    echo ""
    echo "=== PERF ==="
    perf --version
} > "$OUT_DIR/hardware_info.txt"

echo " 2. Compilando as 4 Versoes "
echo "----------------------------------------------------"

# Compilacao, alterando apenas flags de versoes do if-def
gfortran -cpp -O3 -fno-tree-vectorize -DVERSION_BASE "$SRC_FILE" -o stability_base
gfortran -cpp -O3 -fno-tree-vectorize -DVERSION_MEMORIA "$SRC_FILE" -o stability_memoria
gfortran -cpp -O3 -march=native -fopenmp-simd -DVERSION_SIMD "$SRC_FILE" -o stability_simd
gfortran -cpp -O3 -march=native -fopenmp-simd -DVERSION_COMBINADO "$SRC_FILE" -o stability_combinado

echo " 3. Executando Experimentos "
echo "----------------------------------------------------"

VERSOES=("stability_base" "stability_memoria" "stability_simd" "stability_combinado")
EVENTOS="cycles,instructions,L1-dcache-load-misses,L1-dcache-loads,LLC-load-misses,LLC-loads,power/energy-pkg/,power/energy-cores/"

for bin in "${VERSOES[@]}"; do
    if [ -f "./$bin" ]; then
        echo ""
        echo "Executando ./$bin"
        echo "--------------------------------------------------"
        # para repetições do experimento alterar -r 1 para r -x
	sudo taskset -c 0 perf stat -r 1 -a -e $EVENTOS ./"$bin" 2>&1 | tee "$OUT_DIR/${bin}_perf.log"
        echo ""
    fi
done

echo "Medicoes concluidas! Dados salvos em ./$OUT_DIR/"
echo ""
