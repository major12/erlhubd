FOR %%f IN (src/*.erl) DO erlc -b beam -o ebin src/%%f
copy src\*.app ebin\*