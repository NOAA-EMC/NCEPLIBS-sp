set -x
xlf90_r -qsmp=noauto unittest01.f -L/nwprod/lib -lsp_4 -lessl -o unittest01.x4o||exit 1
unittest01.x4o
xlf90_r -qsmp=noauto -qrealsize=8 unittest01.f -L/nwprod/lib -lsp_d -lessl -o unittest01.xdo||exit 1
unittest01.xdo
xlf90_r -qsmp=noauto -qrealsize=8 -qintsize=8 unittest01.f -L/nwprod/lib -lsp_8 -lessl -o unittest01.x8o||exit 1
unittest01.x8o

xlf90_r -qsmp=noauto unittest01.f splath.k4.o lu.kd.o -L/nwprod/lib -lsp_4 -lessl -o unittest01.x4h||exit 1
unittest01.x4h
xlf90_r -qsmp=noauto -qrealsize=8 unittest01.f splath.kd.o lu.kd.o -L/nwprod/lib -lsp_d -lessl -o unittest01.xdh||exit 1
unittest01.xdh
xlf90_r -qsmp=noauto -qrealsize=8 -qintsize=8 unittest01.f splath.k8.o lu.kd.o -L/nwprod/lib -lsp_8 -lessl -o unittest01.x8h||exit 1
unittest01.x8h

xlf90_r -qsmp=noauto unittest01.f splat.k4.o lu.kd.o -L/nwprod/lib -lsp_4 -lessl -o unittest01.x4||exit 1
unittest01.x4
xlf90_r -qsmp=noauto -qrealsize=8 unittest01.f splat.kd.o lu.kd.o -L/nwprod/lib -lsp_d -lessl -o unittest01.xd||exit 1
unittest01.xd
xlf90_r -qsmp=noauto -qrealsize=8 -qintsize=8 unittest01.f splat.k8.o lu.kd.o -L/nwprod/lib -lsp_8 -lessl -o unittest01.x8||exit 1
unittest01.x8
