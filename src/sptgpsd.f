C> @file
C>
C> Transform spectral to polar stereo. gradients
C> @author IREDELL @date 96-02-29
C>
C> this subprogram performs a spherical transform
C> from spectral coefficients of scalar fields
C> to gradient fields on a pair of polar stereographic grids.
C> The wave-space can be either triangular or rhomboidal.
C> The wave and grid fields may have general indexing,
C> but each wave field is in sequential 'ibm order',
C> i.e. with zonal wavenumber as the slower index.
C> the two square polar stereographic grids are centered
C> on the respective poles, with the orientation longitude
C> of the southern hemisphere grid 180 degrees opposite
C> that of the northern hemisphere grid.
C> The vectors are automatically rotated to be resolved
C> relative to the respective polar stereographic grids.
C>
C> The transform is made efficient             \ 4 | 5 /
C> by combining points in eight sectors         \  |  /
C> of each polar stereographic grid,           3 \ | / 6
C> numbered as in the diagram at right.           \|/
C> The pole and the sector boundaries          ----+----
C> are treated specially in the code.             /|\
C> Unfortunately, this approach induces        2 / | \ 7
C> some hairy indexing and code loquacity,      /  |  \
C> for which the developer apologizes.         / 1 | 8 \
C>
C> The transforms are all multiprocessed over sector points.
C> transform several fields at a time to improve vectorization.
C> Subprogram can be called from a multiprocessing environment.
C>
C> PROGRAM HISTORY LOG:
C> -  96-02-29  IREDELL
C> - 1998-12-15  IREDELL  OPENMP DIRECTIVES INSERTED
C>
C> @param IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C>                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param MAXWV    - INTEGER SPECTRAL TRUNCATION
C> @param KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C> @param NPS      - INTEGER ODD ORDER OF THE POLAR STEREOGRAPHIC GRIDS
C> @param KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C>                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C> @param KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C>                (DEFAULTS TO NPS*NPS IF KGSKIP=0)
C> @param NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C>                (DEFAULTS TO 1 IF NISKIP=0)
C> @param NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C>                (DEFAULTS TO NPS IF NJSKIP=0)
C> @param TRUE     - REAL LATITUDE AT WHICH PS GRID IS TRUE (USUALLY 60.)
C> @param XMESH    - REAL GRID LENGTH AT TRUE LATITUDE (M)
C> @param ORIENT   - REAL LONGITUDE AT BOTTOM OF NORTHERN PS GRID
C>                (SOUTHERN PS GRID WILL HAVE OPPOSITE ORIENTATION.)
C> @param WAVE     - REAL (*) WAVE FIELDS
C> @param XN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC X-GRADIENTS
C> @param YN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC Y-GRADIENTS
C> @param XS       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC X-GRADIENTS
C> @param YS       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC Y-GRADIENTS
C>
C> SUBPROGRAMS CALLED:
C>   - SPWGET()       GET WAVE-SPACE CONSTANTS
C>   - SPLAPLAC()     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C>   - SPTGPSV()      TRANSFORM SPECTRAL VECTOR TO POLAR STEREO.
      SUBROUTINE SPTGPSD(IROMB,MAXWV,KMAX,NPS,
     &                   KWSKIP,KGSKIP,NISKIP,NJSKIP,
     &                   TRUE,XMESH,ORIENT,WAVE,XN,YN,XS,YS)

      REAL WAVE(*),XN(*),YN(*),XS(*),YS(*)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      REAL WD((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,KMAX)
      REAL WZ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,KMAX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE PRELIMINARY CONSTANTS
      CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MDIM=2*MX+1
      KW=KWSKIP
      IF(KW.EQ.0) KW=2*MX
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE GRADIENTS
C$OMP PARALLEL DO PRIVATE(KWS)
      DO K=1,KMAX
        KWS=(K-1)*KW
        CALL SPLAPLAC(IROMB,MAXWV,ENN1,WAVE(KWS+1),WD(1,K),1)
        WZ(1:2*MX,K)=0.
      ENDDO
      CALL SPTGPSV(IROMB,MAXWV,KMAX,NPS,MDIM,KGSKIP,NISKIP,NJSKIP,
     &             TRUE,XMESH,ORIENT,WD,WZ,XN,YN,XS,YS)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
