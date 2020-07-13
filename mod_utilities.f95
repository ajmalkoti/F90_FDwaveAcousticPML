MODULE MOD_UTILITIES

CONTAINS
	
	!#####################################################
	SUBROUTINE PAD_ARRAY(MATOUT,MATIN,NAB, TOPBC,PLOTON,NX,NZ)
        USE MOD_PARAM, ONLY: DP
           
        IMPLICIT NONE
        REAL(KIND=DP),INTENT(IN)                    :: MATIN(:,:)
        REAL(KIND=DP),ALLOCATABLE,INTENT(OUT)       :: MATOUT(:,:)
        INTEGER, INTENT(IN)                         :: NAB
        CHARACTER(LEN=*),INTENT(IN)                 :: TOPBC
        CHARACTER(LEN=*),OPTIONAL,INTENT(IN)        :: PLOTON
        INTEGER,OPTIONAL,INTENT(OUT)				:: NX,NZ
    
        INTEGER :: I
        INTEGER :: D1,D2     !NEW DIMENSIONS (AFTER PADDING)
        INTEGER :: S2,S1 	 !OLD DIMENSION (BEFORE PADDING)
        REAL(KIND=DP):: VAL
            
        S1=SIZE(MATIN,1);		!PRINT*,'S1 : ' , S1
        S2=SIZE(MATIN,2);		!PRINT*,'S2 : ' , S2
        
        !PRINT*,'BC TYPE: ',TOPBC
        IF(TOPBC=='TOPABC') THEN  
            
            D1= S1 + 2*NAB
            D2= S2 + 2*NAB
            ALLOCATE(MATOUT(D1,D2))
            MATOUT=0.
            
            MATOUT(NAB+1:D1-NAB ,  NAB+1:D2-NAB)=MATIN   
            
            MATOUT(1:NAB , 1:NAB)=MATIN(1,1)            ! LEFT TOP CORNER
            MATOUT(D1-NAB+1:D1 , 1:NAB)=MATIN(S1,1)     ! LEFT BOTTOM CORNER
            MATOUT(1:NAB , D2-NAB+1:D2)= MATIN(1,S2)    ! LEFT TOP CORNER
            MATOUT(D1-NAB+1:D1 , D2-NAB+1:D2)=MATIN(S1,S2)! RIGHT BOTTOM CORNER
            
            DO I=1,S1
                MATOUT(NAB+I , 1:NAB)= MATIN(I,1)       !LEFT EDGE
                MATOUT(NAB+I , D2-NAB+1:D2)= MATIN(I,S2)  !RIGHT EDGE
            END DO
    
            DO I=1,S2
                MATOUT(1:NAB , NAB+I)= MATIN(1,I)   !TOP EDGE
                MATOUT(D1-NAB+1:D1 , NAB+I)= MATIN(S1,I)  !BOTTOM EDGE
            END DO
    
        ELSE IF (TOPBC=='TOPFS')THEN
            D1= S1 + NAB
            D2= S2 + 2*NAB
            ALLOCATE(MATOUT(D1,D2))
    
            MATOUT=0.
            MATOUT(1:D1-NAB ,  NAB+1:D2-NAB)= MATIN   
            
            MATOUT(D1-NAB+1:D1 , 1:NAB)=MATIN(S1,1)     	! LEFT BOTTOM CORNER
            MATOUT(D1-NAB+1:D1 , D2-NAB+1:D2)=MATIN(S1,S2)	! RIGHT BOTTOM CORNER
            
            DO I=1,S1
                MATOUT(I , 1:NAB)= MATIN(I,1)           ! LEFT EDGE
                MATOUT(I , D2-NAB+1:D2)= MATIN(I,S2)    ! RIGHT EDGE
            END DO
    
            DO I=1,S2
                MATOUT(D1-NAB+1:D1 , NAB+I)= MATIN(S1,I)  !BOTTOM EDGE
            END DO
      
        END IF  

        IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))THEN
            CALL PLT_MAT(MATOUT,'PADDED MATRIX','X','Z',5)
        END IF

        IF(PRESENT(NZ)) NZ=D1
        IF(PRESENT(NX)) NX=D2

	 END SUBROUTINE PAD_ARRAY
	!#####################################################

	SUBROUTINE PLT_MAT_F(FNAME,TITLE,XLABEL,YLABEL,WTTIME)
        USE MOD_PARAM
        IMPLICIT NONE
        CHARACTER(LEN=*)    		:: FNAME
        CHARACTER(LEN=*),OPTIONAL	:: TITLE,XLABEL,YLABEL
		INTEGER, OPTIONAL			:: WTTIME

        
		OPEN(UNIT=20,FILE='TEMP.GNU',ACTION='WRITE')
        IF (PRESENT(TITLE)) WRITE(20,*) 'set title "',TITLE,'"'
        IF (PRESENT(XLABEL)) WRITE(20,*) 'set xlabel "',XLABEL,'"'          
        IF (PRESENT(YLABEL)) WRITE(20,*) 'set ylabel "',YLABEL,'"'
        WRITE(20,*) 'set yrange [:] reverse '  
		WRITE(20,*) 'plot "',FNAME,'" matrix with image'
        IF (PRESENT(WTTIME)) WRITE(20,*) 'pause(',WTTIME,')'
        CLOSE(20)
    	CALL SYSTEM('gnuplot TEMP.GNU')    
    END SUBROUTINE PLT_MAT_F
	
	
	SUBROUTINE PLT_VEC_F(FNAME,TITLE,XLABEL,YLABEL,WTTIME)
        USE MOD_PARAM
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN)        :: FNAME
        CHARACTER(LEN=*), INTENT(IN),OPTIONAL   :: TITLE,XLABEL,YLABEL
        INTEGER, INTENT(IN), OPTIONAL           :: WTTIME
        
        OPEN(UNIT=20,FILE='TEMP.GNU',ACTION='WRITE')
        IF (PRESENT(TITLE)) WRITE(20,*) 'set title "',TITLE,'"'
        IF (PRESENT(XLABEL)) WRITE(20,*) 'set xlabel "',XLABEL,'"'          
        IF (PRESENT(YLABEL)) WRITE(20,*) 'set ylabel "',YLABEL,'"'
        !WRITE(20,*) 'set yrange [:] reverse '  
        WRITE(20,*) 'plot "',FNAME,'" using 1 with lines'
        IF (PRESENT(WTTIME)) WRITE(20,*) 'pause(',WTTIME,')'
        CLOSE(20)
        CALL SYSTEM('gnuplot TEMP.GNU')    
    END SUBROUTINE PLT_VEC_F
    

	SUBROUTINE PLT_MAT(MAT,TITLE,XLABEL,YLABEL,WTTIME)
        USE MOD_PARAM
        USE MOD_READ_WRITE;
        IMPLICIT NONE
        REAL(KIND=DP), INTENT(IN)				:: MAT(:,:)
        CHARACTER(LEN=*), INTENT(IN),OPTIONAL	:: TITLE,XLABEL,YLABEL
		INTEGER, INTENT(IN), OPTIONAL			:: WTTIME

        
		OPEN(UNIT=20,FILE='TEMP.DAT',ACTION='WRITE')
		CALL WRITE_MAT(MAT,'TEMP.DAT')
        CLOSE(20)
    	CALL PLT_MAT_F('TEMP.DAT',TITLE,XLABEL,YLABEL,WTTIME)
    END SUBROUTINE PLT_MAT	


    SUBROUTINE PLT_VEC(VEC,TITLE,XLABEL,YLABEL,WTTIME)
        USE MOD_PARAM
        USE MOD_READ_WRITE;
        IMPLICIT NONE
        
        REAL(KIND=DP), INTENT(IN)               :: VEC(:)
        CHARACTER(LEN=*), INTENT(IN),OPTIONAL   :: TITLE,XLABEL,YLABEL
        INTEGER, INTENT(IN), OPTIONAL           :: WTTIME
        INTEGER     ::  I
  
        OPEN(UNIT=20,FILE='TEMP.DAT',ACTION='WRITE')
		CALL WRITE_VEC(VEC,'TEMP.DAT')
        CLOSE(20)
    	CALL PLT_VEC_F('TEMP.DAT',TITLE,XLABEL,YLABEL,WTTIME)
    END SUBROUTINE PLT_VEC

    
    
END MODULE MOD_UTILITIES