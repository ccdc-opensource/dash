import sys
import subprocess

if __name__ == "__main__":
    with open("E:/junk.txt",'w') as w:
        w.write("Holding stub for ZMatrix until we can expose through the CSD python API")
        w.write(": " + str(sys.argv[1:]) + "\n")

    to_run = ["makezmatrix.exe"] + sys.argv[1:]
    x = subprocess.run(to_run)
    sys.exit(x)

