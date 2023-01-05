C> @file
C> Set number of cpus.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|---------
C> 94-08-19 | IREDELL | Initial.
C> 98-11-09 | VUONG | ADD DOC>BLOCK AND REMOVE CRAY REFERENCES
C> 1998-12-18 | IREDELL | IBM SMP VERSION
C> 2010-11-16 | SLOVACEK | LINUX MUST HAVE DIFFERENT CALL
C> 2012-11-01 | MIRVIS | Multi- threading on LINUX-IBM/TIDE
C>
C> @author IREDELL @date 94-08-19

C> Set number of cpus - the number of processors over which
C> to parallelize.
C>
C> @param[out] ncpus  number of cpus
C>
C>
C> @author IREDELL @date 94-08-19
      FUNCTION NCPUS()
      INTEGER NTHREADS, TID, OMP_GET_NUM_THREADS,OMP_GET_THREAD_NUM
C    Obtain thread number
#ifdef OPENMP
!$OMP PARALLEL PRIVATE(TID)
      TID = OMP_GET_THREAD_NUM()
!      PRINT *, '...............thread # ', TID
	if (TID. eq. 0) then
        NCPUS=OMP_GET_NUM_THREADS()
!        PRINT *, 'totaly #------------------- of threads = ',NCPUS
	endif
!$OMP END PARALLEL
#else
      TID = 0
      NCPUS = 1
#endif
      RETURN
      END
