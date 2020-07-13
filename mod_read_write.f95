MODULE MOD_READ_WRITE
!INCLUDED SUBROUTINES
!READ_IP_VAR_INT
!READ_IP_VAR_REAL
!READ_IP_VAR_STR
!READ_MAT_FILE
!WRITE_MAT
!WRITE_VEC
!PRINT_MAT
!PRINT_VEC


CONTAINS
	!######################################################
    SUBROUTINE READ_IP_VAR_INT(LINENO,NUM)
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::  LINENO
        INTEGER, INTENT(OUT)::  NUM
        CHARACTER(100) :: LINE
        CHARACTER(LEN=:),ALLOCATABLE :: LINE_TRIM
        INTEGER        :: I,IPOS
          
        OPEN(UNIT=10,FILE="IP.DAT",ACTION="READ")
        DO I=1,LINENO
            READ(10,'(A)') LINE
        END DO 
        PRINT*, LINE
        IPOS = SCAN(LINE,"=",BACK=.TRUE.)
        LINE_TRIM = TRIM(LINE(IPOS+1:))
        READ(LINE_TRIM, '(I20)' ) NUM            
        CLOSE(10)
    END SUBROUTINE
    
    
	!######################################################
   	SUBROUTINE READ_IP_VAR_REAL(LINENO,NUM)
		USE MOD_PARAM, ONLY: DP
      	IMPLICIT NONE
        INTEGER, INTENT(IN)	::	LINENO
        REAL(KIND=DP), INTENT(OUT)::  NUM
        
        CHARACTER(100) :: LINE
        CHARACTER(LEN=:),ALLOCATABLE :: LINE_TRIM
        INTEGER		   :: I,IPOS
          
        OPEN(UNIT=10,FILE="IP.DAT",ACTION="READ")
        DO I=1,LINENO
          	READ(10,'(A)') LINE
		END DO
        
        PRINT*, LINE
        IPOS = SCAN(LINE,"=",BACK=.TRUE.)
        LINE_TRIM= TRIM(LINE(IPOS+1:))
        !PRINT*,LINE_TRIM
		READ( LINE_TRIM, * ) NUM            
   		CLOSE(10)
    END SUBROUTINE 

    
	!######################################################
    FUNCTION READ_IP_VAR_STR(LINENO)
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::  LINENO
        CHARACTER(LEN=:),ALLOCATABLE ::  READ_IP_VAR_STR
        
        CHARACTER(100) :: LINE
        INTEGER        :: I,IPOS
          
        OPEN(UNIT=10,FILE="IP.DAT",ACTION="READ")
        DO I=1,LINENO
            READ(10,'(A)') LINE
        END DO 
        PRINT*, LINE
        IPOS = SCAN(LINE,"=",BACK=.TRUE.)
        READ_IP_VAR_STR= TRIM(LINE(IPOS+2:))    ! TRIM DOESN'T REMOVES ONLY TRAILING SPACES
        !PRINT*, 'IN SUBROUTINE', READ_IP_VAR_STR
        CLOSE(10)
    END FUNCTION READ_IP_VAR_STR
	
 	!######################################################
    SUBROUTINE READ_MAT_FILE(MAT,FILENAME)
        USE MOD_PARAM, ONLY:DP
        IMPLICIT NONE
        REAL(KIND=DP),DIMENSION(:,:), INTENT(INOUT) :: MAT
        CHARACTER(LEN=*), INTENT(IN) :: FILENAME
        INTEGER         :: I,J
     
		OPEN(UNIT=20,FILE=FILENAME,ACTION='READ')
        DO I=1,SIZE(MAT,1)
            READ(20,*) MAT(I,:)
        END DO
        CLOSE(20)
    END SUBROUTINE READ_MAT_FILE    
	
	!######################################################
    SUBROUTINE WRITE_MAT(MAT,FILENAME)
        USE MOD_VARIABLES, ONLY: NX,NZ,DP
        IMPLICIT NONE
        REAL(KIND=DP),DIMENSION(:,:), INTENT(IN) :: MAT
        CHARACTER(LEN=*), INTENT(IN) :: FILENAME
        INTEGER         :: I,J
 
        OPEN(UNIT=20,FILE=FILENAME,ACTION='WRITE')
        DO I=1,SIZE(MAT,1)
            DO J=1,SIZE(MAT,2)
                WRITE(20,'(F22.15)', ADVANCE='NO') MAT(I,J)
            END DO
            WRITE(20,*) ''
        END DO
        CLOSE(20)
    END SUBROUTINE WRITE_MAT

	!######################################################
    SUBROUTINE WRITE_VEC(VEC,FILENAME)
        USE MOD_VARIABLES, ONLY: NX,NZ,DP
        IMPLICIT NONE
        REAL(KIND=DP),DIMENSION(:), INTENT(IN) :: VEC
        CHARACTER(LEN=*), INTENT(IN) :: FILENAME
        INTEGER         :: I,J
 
        OPEN(UNIT=20,FILE=FILENAME,ACTION='WRITE')
        DO I=1,SIZE(VEC,1)
            WRITE(20,'(F22.15)', ADVANCE='NO') VEC(I)
            WRITE(20,*) ''
        END DO
        CLOSE(20)
    END SUBROUTINE WRITE_VEC

 	!######################################################
    SUBROUTINE PRINT_MAT(STR, MAT)
    	! MAT: MATRIX TO BE PRINTED
        ! STR: MESSAGE TO BE DISPLAYED ON SCREEN BEFORE PRINTING MATRIX
        USE MOD_PARAM, ONLY:DP
        IMPLICIT NONE
        REAL(KIND=DP), INTENT(IN)	:: MAT(:,:)
        CHARACTER(LEN=*),INTENT(IN)	:: STR
        INTEGER:: I,J
		
		PRINT*,'----------------------------------------'
		PRINT*, STR
        DO I=1,SIZE(MAT,1)
          	DO J=1,SIZE(MAT,2)
         		WRITE(*,'(F10.3)',ADVANCE='NO'), MAT(I,J)
            END DO
            WRITE(*,*),''
	    END DO
    END SUBROUTINE PRINT_MAT

 	!######################################################
    SUBROUTINE PRINT_VEC(STR, VEC)
    	! VEC: VECTOR TO BE PRINTED
        ! STR: MESSAGE TO BE DISPLAYED ON SCREEN BEFORE PRINTING MATRIX
        USE MOD_PARAM, ONLY:DP
        IMPLICIT NONE
        REAL(KIND=DP), INTENT(IN)	:: VEC(:)
        CHARACTER(LEN=*),INTENT(IN)	:: STR
        INTEGER:: I,J
		
		PRINT*,'----------------------------------------'
		PRINT*, STR
        DO I=1,SIZE(VEC,1)
          	WRITE(*,'(F10.3)',ADVANCE='NO') VEC(I)
	    END DO
		WRITE(*,*) ''
    END SUBROUTINE PRINT_VEC
	
END MODULE MOD_READ_WRITE