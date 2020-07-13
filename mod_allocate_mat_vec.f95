MODULE MOD_ALLOCATE_MAT_VEC

CONTAINS
	
    SUBROUTINE ALLOCATE_VEC(STR,VEC,N)           
        IMPLICIT NONE
        INTEGER,INTENT(IN)            :: N
        CHARACTER(len=*),INTENT(IN)   ::  STR
        REAL, DIMENSION(:), ALLOCATABLE,INTENT(INOUT)     :: VEC
        INTEGER                       :: OK
                            
        ALLOCATE(VEC(N),STAT=OK)
        IF(OK/=0) THEN
          PRINT*, 'ALLOCATION ERROR FOR "', TRIM(STR), '"'
          STOP
        END IF
        VEC=0
    END SUBROUTINE ALLOCATE_VEC

    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    SUBROUTINE ALLOCATE_MAT(STR,MAT,NV,NH)
    	USE MOD_PARAM , ONLY:DP
        IMPLICIT NONE
        INTEGER,INTENT(IN)            ::  NV,NH
        CHARACTER(len=*),INTENT(IN)   ::  STR
        REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE,INTENT(INOUT)   ::  MAT            
        INTEGER                       ::  OK
        
        ALLOCATE(MAT(NV,NH),STAT=OK)
        IF(OK/=0) THEN
            PRINT*, 'ALLOCATION ERROR FOR "', TRIM(STR), '"'
            STOP
        END IF
        MAT=0

   END SUBROUTINE ALLOCATE_MAT
   
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
END MODULE MOD_ALLOCATE_MAT_VEC