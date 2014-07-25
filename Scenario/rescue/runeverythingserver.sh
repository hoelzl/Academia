echo "!!  You can call \`bash runplatonsmind.sh\` before running this script on a separate terminal and keep it alive"
echo "!!    over multiple runs of this script, which will greatly speed up an ensemble test. CHARON will"
echo "!!    automatically detect if platonsmind is already running!"
echo ""
lua ../../Sources/Lua/hades/charon.lua ./world.lua --hadeslog hadeslog.txt --psychelog psychelog.txt -S
