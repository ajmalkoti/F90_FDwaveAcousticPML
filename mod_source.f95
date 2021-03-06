MODULE MOD_SOURCE
CONTAINS
SUBROUTINE SOURCE_RICKER(S)
    USE MOD_PARAM
    IMPLICIT NONE
    TYPE(SOURCE) :: S   
    INTEGER         :: I
    REAL(KIND=DP)   :: PFT,TAU
    
    S%NT=FLOOR(S%T/S%DT)
    ALLOCATE(S%SIG(S%NT))
    DO I=1,S%NT
       TAU=(I-1)*S%DT - S%T0
       PFT= (PI**2)* (S%F0**2)*(TAU**2)
       S%SIG(I)=(1-2*PFT)*EXP(-PFT)
    END DO
END SUBROUTINE SOURCE_RICKER


SUBROUTINE SOURCE_GAUSSIAN(S)
    USE MOD_PARAM
    IMPLICIT NONE
    TYPE(SOURCE) :: S   
    INTEGER         :: I
    REAL(KIND=DP)   :: PFT,TAU
    
    S%NT=FLOOR(S%T/S%DT)
    ALLOCATE(S%SIG(S%NT))
    DO I=1,S%NT
       TAU=(I-1)*S%DT - S%T0
       PFT= (PI**2)* (S%F0**2)*(TAU**2)
       S%SIG(I)= EXP(-PFT)
    END DO
END SUBROUTINE SOURCE_GAUSSIAN


END MODULE MOD_SOURCE