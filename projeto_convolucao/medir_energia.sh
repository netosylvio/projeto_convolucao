#!/bin/bash

set -e

if [ "$#" -lt 1 ]; then
    echo "Uso: $0 EXECUTAVEL [ARGUMENTOS...]"
    exit 1
fi

ENERGY_FILE="/sys/class/powercap/intel-rapl:0/energy_uj"
MAX_FILE="/sys/class/powercap/intel-rapl:0/max_energy_range_uj"

if [ ! -r "$ENERGY_FILE" ] && ! sudo -n test -r "$ENERGY_FILE"; then
    echo "Erro: nao foi possivel ler o contador RAPL."
    exit 1
fi

read_energy() {
    sudo -n cat "$ENERGY_FILE"
}

MAX_RANGE=$(sudo -n cat "$MAX_FILE")
ENERGY_BEFORE=$(read_energy)

START_NS=$(date +%s%N)

echo "========== EXECUCAO =========="
echo "Programa: $*"

"$@" > /tmp/resultado_energia.txt
cat /tmp/resultado_energia.txt

END_NS=$(date +%s%N)
ENERGY_AFTER=$(read_energy)

ELAPSED_NS=$((END_NS - START_NS))

# Trata a possibilidade de o contador RAPL dar a volta.
if [ "$ENERGY_AFTER" -ge "$ENERGY_BEFORE" ]; then
    DELTA_UJ=$((ENERGY_AFTER - ENERGY_BEFORE))
else
    DELTA_UJ=$((MAX_RANGE - ENERGY_BEFORE + ENERGY_AFTER))
fi

echo
echo "========== MEDICAO ENERGETICA =========="
echo "Energia inicial: ${ENERGY_BEFORE} uJ"
echo "Energia final: ${ENERGY_AFTER} uJ"
echo "Energia consumida: $(awk -v e="$DELTA_UJ" 'BEGIN {printf "%.6f", e/1000000}') J"
echo "Tempo total externo: $(awk -v t="$ELAPSED_NS" 'BEGIN {printf "%.6f", t/1000000000}') s"
echo "Potencia media do pacote: $(awk -v e="$DELTA_UJ" -v t="$ELAPSED_NS" 'BEGIN {if (t>0) printf "%.3f", e*1000/t; else print "indisponivel"}') W"
echo "========================================"
