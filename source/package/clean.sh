rm -rf .DS_Store
rm -rf *.or
rm -rf *.bak
rm -rf *.exe
rm -rf *.ppu
rm -rf *.o
rm -rf *.compiled
rm -rf lib/*

find . -type f -name *.~ -delete
find . -type f -name *.o -delete
find . -type f -name *.oe -delete
find . -type f -name *.rst -delete
find . -type f -name *.bak -delete
find . -type f -name *.ppu -delete
find . -type f -name *.compiled -delete
find . -type d -name backup -exec rm -rf "{}" +

rm -rf lib
mkdir lib
touch lib/empty

