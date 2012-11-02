set -x
#xlf90 -c lu.f -o lu.k4.o
#xlf90 -c -qrealsize=8 lu.f -o lu.kd.o
#xlf90 -c -qintsize=8 -qrealsize=8 lu.f -o lu.k8.o
#xlf -c splat.f -o splat.k4.o
#xlf -c -qrealsize=8 splat.f -o splat.kd.o
#xlf -c -qintsize=8 -qrealsize=8 splat.f -o splat.k8.o
xlf -c splath.f -o splath.k4.o
xlf -c -qrealsize=8 splath.f -o splath.kd.o
xlf -c -qintsize=8 -qrealsize=8 splath.f -o splath.k8.o
