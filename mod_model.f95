MODULE MOD_MODEL

	CONTAINS
   	!#################################################################
    SUBROUTINE MODEL_READ_FILE(MAT,FNAME,NX,NZ,FPRINT)
		!MAT	(O/P)	=	THE MATRIX FROM FILE WILL BE READ IN THIS FILE
        !FNAME  (I/P)	=   NAME OF FILE TO BE READ FOR MODEL
        !NX		(I/P)	=	NO OF DATA VALUES IN A ROW IN FILE (X-DIRECTION)
        !NZ		(I/P)	=   NO OF ROWS  IN FILE (Z-DIRECTION)
        !
		!CALL MODEL_READ_FILE(M%VP,'VP.TXT',M%NX,M%NZ,'N')
		!CALL PFILE('VP.TXT','MODEL','X','Z',10)
        
        USE MOD_PARAM
        USE MOD_ALLOCATE_MAT_VEC
        USE MOD_READ_WRITE
        
        IMPLICIT NONE
 		REAL(KIND=DP),ALLOCATABLE,	INTENT(OUT)		::	MAT(:,:)
		CHARACTER(LEN=*), INTENT(IN) 	:: FNAME
        INTEGER, INTENT(IN)				:: NX,NZ
        CHARACTER,OPTIONAL,INTENT(IN)	:: FPRINT
	       
        CALL ALLOCATE_MAT("",MAT,NZ,NX);		!PRINT*, 'SIZE IS :', SIZE(M%VP,1),SIZE(M%VP,2)
		CALL READ_MAT_FILE(MAT,FNAME);          
		
		IF (PRESENT(FPRINT).AND.((FPRINT=='Y').OR.(FPRINT=='y'))) THEN        
        	CALL PRINT_MAT('VP MATRIX IS',MAT)
		END IF
        
    END SUBROUTINE MODEL_READ_FILE
        
 	!#################################################################   
	SUBROUTINE MODEL_N_LAYER(MAT,NX,NZ,DH,HTV_RATIO,DEPTHV,VEC, FNAME,PLOTON)
		!DH			(I/P)	= GRID SPACING
        !HTV_RATIO	(I/P)	= HORIZONTAL TO VERITCAL RATIO
        !DEPTHV		(I/P)	= A VECTOR FOR DEPTH OF EACH LAYER E.G. (/200, 400,600/) REPRESENT THREE LAYERS
		!VEC		(I/P)	= VALUES FOR RESPECTIVE LAYERS (OF A GIVEN PROPERTY)
        !MAT		(O/P)	= THE MATRIX MODEL OF GIVEN PROPERTY
        !NX,NZ		(O/P),(OPTIONAL) 	= SIZE OF MODEL 
        !FNAME		(I/P),(OPTIONAL)	= NAME OF FILE TO SAVE THE MATRIX MODEL
    
    USE MOD_PARAM
    USE MOD_READ_WRITE
    USE MOD_UTILITIES
    IMPLICIT NONE
    REAL(KIND=DP), ALLOCATABLE,INTENT(OUT)  :: MAT(:,:)
	INTEGER,OPTIONAL,INTENT(OUT)			:: NX,NZ
    
    REAL(KIND=DP), INTENT(IN)				:: DH
    REAL(KIND=DP), INTENT(IN)               :: HTV_RATIO    !HORIZONTAL TO VERTICAL RATIO
    REAL(KIND=DP), INTENT(IN)               :: DEPTHV(:),VEC(:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)	:: FNAME,PLOTON
    
    INTEGER                 :: I, N, D2, D1
    INTEGER, ALLOCATABLE    :: DEPTHIN(:)		! DEPTH OF LAYERS IN INTEGERS

    !------------------------------------------------------------  
	!PRINT*,'FLAG : ',DEPTHV;			PRINT*,'FLAG : ',VEC
    IF (SIZE(DEPTHV)/=SIZE(VEC))THEN
    	PRINT*,'MODEL_N_LAYER'
    	PRINT*, 'ERROR: DIMENSIONS OF VECTOR MISMATCH'
        STOP
    END IF

    DO I=1,SIZE(DEPTHV)-1
        IF (DEPTHV(I)>=DEPTHV(I+1)) THEN 
	    	PRINT*,'MODEL_N_LAYER'
            PRINT*, 'ERROR: WRONGLY ASSIGNED VALUES IN DEPTH VECTOR.'
            PRINT*, 'SUGGESTION: THE VALUES IN DEPTH VECTOR SHOULD BE IN INCREASING ORDER.'
            STOP
        END IF
    END DO
    !-------------------------------------------------------------
	!SIZE OF THE MODEL
    N= SIZE(VEC);					  !PRINT*,'FLAG0,N', N
    D1=NINT(DEPTHV(N)/DH);		      !PRINT*,'FLAG1, D1', D1
    D2=NINT(HTV_RATIO*DEPTHV(N)/DH);  !PRINT*,'FLAG2, D2', D2
    ALLOCATE(MAT(D1,D2))
	IF (PRESENT(NX)) NX=D2
    IF (PRESENT(NZ)) NZ=D1			

    ! NO OF LAYERS IN MODEL
    ALLOCATE(DEPTHIN(N))         ! DEPTH OF LAYERS IN INTEGERS
    DEPTHIN=NINT(DEPTHV(:)/DH)   !PRINT*,'D', DEPTHIN
	    

    MAT=0. ;						!INITIALIZE MATRIX            
    MAT(1:DEPTHIN(1),:)=VEC(1)		!ASSIGN VALUES ONLY TO FIRST LAYER
    IF(N>3)THEN						!ASSIGN VALUES UPTO 2 TO N-1 LAYERS
    	DO I=2,N
	        MAT(DEPTHIN(I-1):DEPTHIN(I),:)=VEC(I)
    	END DO
    END IF
    MAT(DEPTHIN(N-1):DEPTHIN(N),:)=VEC(N)      !ASSIGN VALUES ONLY TO LAST LAYER
    

	!CALL PRINT_MAT('MATRIX : ',MAT)

   	IF(PRESENT(FNAME))THEN
		CALL WRITE_MAT(MAT,FNAME//'.TXT')
        PRINT*, 'FILE WRITTEN ON DISK:', FNAME
    END IF

    IF (PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))THEN
      CALL PLT_MAT( MAT, FNAME,'X','Z',2)
	END IF
    
    END SUBROUTINE MODEL_N_LAYER


    !#####################################################
	SUBROUTINE INTERPOLATE_MODEL(M,MXH,MZH,PLOTON)
    	USE MOD_PARAM, ONLY: DP
        USE MOD_UTILITIES
        
        IMPLICIT NONE
        REAL(KIND=DP), INTENT(IN)			 	:: M(:,:)
        REAL(KIND=DP),ALLOCATABLE,INTENT(OUT)	:: MXH(:,:),MZH(:,:)
        CHARACTER(LEN=*),OPTIONAL,INTENT(IN)        :: PLOTON
        
		INTEGER	:: D1,D2

        D1=SIZE(M,1)
        D2=SIZE(M,2)
		
        ALLOCATE(MXH(D1,D2))
        ALLOCATE(MZH(D1,D2))
        !PRINT*,'MXH SIZE  :', SIZE(MXH,1)
		MXH(:,:)=0.
        MZH(:,:)=0.
        
		MZH(2:D1,:) = .5*(  M(1:(D1-1),:) + M(2:D1,:)  )
		MZH(1,:)= M(1,:)
        
        MXH(:,2:D2) = .5*(  M(: , 1:(D2-1)) + M(:,2:D1)  )
		MXH(:,1) = M(:,1)
        
        IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))THEN
            CALL PLT_MAT(MXH,'INTERPOLATED MODEL, ALNG X','X','Z',5)
            CALL PLT_MAT(MZH,'INTERPOLATED MODEL, ALNG Z','X','Z',5)
        END IF

    END SUBROUTINE INTERPOLATE_MODEL
 	!#################################################################


   	!#################################################################
END MODULE MOD_MODEL