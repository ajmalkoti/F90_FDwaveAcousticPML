MODULE MOD_PML
	!-------------------------------------------------------	
    CONTAINS
	!-------------------------------------------------------    
    SUBROUTINE PML_2D(M,BC,PLOTON)!DH,BC%TOPBC,NX,NZ,NAB,BC%SX,BC%SZ,BC%SXH,BC%SZH)
    	USE MOD_PARAM
    	USE MOD_UTILITIES
        IMPLICIT NONE
        TYPE(MODEL), INTENT(IN)	:: M
        TYPE(BOUNDARY_PML), INTENT(INOUT)	:: BC
        CHARACTER(LEN=*), OPTIONAL,INTENT(IN)		:: PLOTON
        
		INTEGER 		:: D1,D2,I
        REAL(KIND=DP) 	:: SIGMA, MAXVP
        CHARACTER 		:: DUMMY
        
    	IF (BC%TOPBC=='TOPABC') THEN
			PRINT*, 'ABC type bounday is chosen for all sides.'
            D1= M%NZ + 2*BC%NAB
        	D2= M%NX + 2*BC%NAB
		ELSE IF(BC%TOPBC=='TOPFS') THEN
			PRINT*, 'FS type bounday is chosen for top surface.'
            D1= M%NZ + BC%NAB
        	D2= M%NX + 2*BC%NAB
		ELSE
            PRINT*, 'BC%TOPBC value has to be either 0 or 1'
            PRINT*, 'Retry with correct value. Press k and hit return'
            READ*, DUMMY
            STOP
		END IF
  
        !FIRST DEFINE THE SIMPLE SIGMA VALUES
		ALLOCATE(BC%SX(D1,D2),BC%SZ(D1,D2),BC%SXH(D1,D2),BC%SZH(D1,D2))
		BC%SX=0.
        BC%SZ=0.
		BC%SXH=0.
        BC%SZH=0.

        !PRINT*,'FLAGGGG. :', BC%TOPBC
		MAXVP=MAXVAL(M%VP)	
        IF (BC%TOPBC=='TOPABC') THEN
            DO I=1, BC%NAB
                SIGMA= ESTIMATE_SIGMA((BC%NAB-I+1)*M%DH,(BC%NAB-1)*M%DH, MAXVP,M%DH, BC%NAB) 
                BC%SX(:,I)=SIGMA         ! LEFT
                BC%SZ(I,:)=SIGMA         ! TOP
                BC%SX(:,D2-I+1)=SIGMA    ! RIGHT
                BC%SZ(D1-I+1,:)=SIGMA  ! BOTTOM
                
                SIGMA= ESTIMATE_SIGMA((BC%NAB-I+.5)*M%DH,(BC%NAB-1)*M%DH, MAXVP,M%DH, BC%NAB) 
                BC%SXH(:,I)=SIGMA         ! LEFT
                BC%SZH(I,:)=SIGMA         ! TOP
                BC%SXH(:,D2-I+1)=SIGMA    ! RIGHT
                BC%SZH(D1-I+1,:)=SIGMA    ! BOTTOM
            END DO
        ELSE IF (BC%TOPBC=='TOPFS')THEN
            DO I=1,BC%NAB
                SIGMA= ESTIMATE_SIGMA((BC%NAB-I+1)*M%DH,(BC%NAB-1)*M%DH, MAXVP,M%DH, BC%NAB) 
                BC%SX(:,I)=SIGMA         ! LEFT
                !BC%SZ(I,:)=SIGMA         ! TOP
                BC%SX(:,D2-I+1)=SIGMA    ! RIGHTasdf
                BC%SZ(D1-I+1,:)=SIGMA  	 ! BOTTOM
                
                SIGMA= ESTIMATE_SIGMA((BC%NAB-I+.5)*M%DH,(BC%NAB-1)*M%DH, MAXVP,M%DH, BC%NAB) 
                BC%SXH(:,I)=SIGMA         ! LEFT
                !BC%SZH(I,:)=SIGMA         ! TOP
                BC%SXH(:,D2-I+1)=SIGMA    ! RIGHT
                BC%SZH(D1-I+1,:)=SIGMA    ! BOTTOM
            END DO
        END IF
		
		! NOW ESTIMATE THE OTHER CONSTANTS
        
  		IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))THEN
            CALL PLT_MAT(BC%SX,'BOUNDARY CONDITIONS SX','X','Z',5)
            CALL PLT_MAT(BC%SXH,'BOUNDARY CONDITIONS SXH','X','Z',5)
            CALL PLT_MAT(BC%SZ,'BOUNDARY CONDITIONS SZ','X','Z',5)
            CALL PLT_MAT(BC%SZH,'BOUNDARY CONDITIONS SZH','X','Z',5)
		END IF
    END SUBROUTINE PML_2D
    
	!------------------------------------------------------- 
    FUNCTION ESTIMATE_SIGMA(X,L,MAXVP,DH,NAB) 
        USE MOD_PARAM, ONLY:DP
        IMPLICIT NONE
        INTEGER, INTENT(IN)		 :: NAB
        REAL(KIND=DP),INTENT(IN) :: X,L, MAXVP,DH
        REAL(KIND=DP)           :: SIGMA0,ESTIMATE_SIGMA
        
        SIGMA0=  -3.*MAXVP*LOG(10.**(-3.))/(2.0*(NAB-1)*DH)
        ESTIMATE_SIGMA=SIGMA0*(X/L)**2
    END FUNCTION ESTIMATE_SIGMA

	!-------------------------------------------------------    
    
END MODULE MOD_PML