MODULE MOD_DERIVATIVE_COMPACT

CONTAINS

!########################################################################

SUBROUTINE DER_COMPACT_2D_CD1O6(DER,A,DIR)
	! 2D (C)ENTRAL (D)ERIVATIVE ORDER= 1; OF ACCURACY (O)RDER=1
	USE MOD_PARAM
    USE MOD_UTILITIES;
    
    IMPLICIT NONE
    REAL(KIND=DP),ALLOCATABLE, INTENT(OUT) :: DER(:,:)
    REAL(KIND=DP), INTENT(IN) 			   :: A(:,:)
	INTEGER,INTENT(IN)					   :: DIR	   !DIRECTION OF DERIVATIVE

	INTEGER :: D1, D2, I, N=100	
    REAL(KIND=DP), ALLOCATABLE:: X(:),Y(:),YD(:),YD_EXACT(:)
    	
    ALLOCATE(X(N),Y(N),YD(N),YD_EXACT(N))

    DO I=1,N
         X(I)=(I-1)* (5.0*PI)/(N-1);          Y(I)= SIN(X(I)); 		 YD_EXACT(I)= COS(X(I))         
		 !X(I)=(I-N/2.) /20;	         Y(I)= EXP(-X(I)*X(I) )      
    END DO
     
	!CALL PLT_VEC(X,'VECTOR X','X','Y',5)
    CALL PLT_VEC(Y,'VECTOR Y','X','Y',5)
	CALL DER_COMPACT_VEC(YD, Y)
    YD=YD/((5.0*PI)/(N-1))
    CALL PLT_VEC(YD,'VECTOR Y','X','Y',5)
    


END SUBROUTINE DER_COMPACT_2D_CD1O6

!########################################################################

SUBROUTINE DER_COMPACT_VEC(YD, Y)
        ! WE WILL BE SOLVING THE A [Y']=B[Y]      OR       A[YD] = B[Y]  
        ! THE SYSTEM IS SOLVED IN TWO STEPS:
        !      DETERMINE 1: Y INTERMEDIATE:      [YI] = B[Y]           
        !      DETERMINE 2: Y DERIVATIVE  :     [YD] = A^(-1) [YI]

        ! LHSMAT    : A TRI-DIGONAL MATRIX (WITH BC), AND
        ! RHSMAT_VEC: TO SAVE SPACE, MAT B IS DECOMPOSED INTO THREE MATRICES (BLOCK MATRIX)
        !             RHSMAT_A, RHSMAT_B, RHSMAT_C
        USE MOD_PARAM
        USE MOD_UTILITIES
        USE MOD_READ_WRITE
        IMPLICIT NONE
        REAL(KIND=DP), INTENT(IN)  :: Y(:)
        REAL(KIND=DP), INTENT(OUT) :: YD(:)

        INTEGER             		 :: I,J,N
        REAL(KIND=DP)                :: ALPHA = 1./3.
        REAL(KIND=DP), ALLOCATABLE   :: YI(:),  LHSMAT(:,:), RHSMAT_A(:,:),  RHSMAT_B(:,:), RHSMAT_C(:,:)

        N=SIZE(Y)
        ALLOCATE(LHSMAT(N,3),RHSMAT_A(2,6),RHSMAT_B(1,5),RHSMAT_C(2,6), YI(N) )
        
        !-------CREATE TRIDIGONAL MATRIX (ONLY THREE DIAGONALS)
        LHSMAT=0.
        FORALL(I=1:N)   LHSMAT(I,2)=1.        !MAKES DIAGONAL =1
        FORALL(I=3:N-2) LHSMAT(I,3)=ALPHA   !MAKES UPPER DIAGONAL =1/3
        FORALL(I=3:N-2) LHSMAT(I,1)=ALPHA   !MAKES LOWER DIAGONAL =1/3
        !-------NOW APPLY BC TO LHS TRIDAG MATRIX

        LHSMAT(1,3)=5.
        LHSMAT(2,1:3)=(/1./8., 1., 3./4. /)
        LHSMAT(N-1,1:3)=  (/ 3./4., 1.,1./8. /)
        LHSMAT(N,1)=5.
        !CALL print_mat('LHS MATRIX IS: ',LHSMAT)
        
        !-------CREATE RHS MATRIX--------------
        RHSMAT_A(1,1:6)= (/ -197./60., -5./12., 5., -5./3., 5./12., -1./20. /)
        RHSMAT_A(2,1:6)= (/ -43./96., -5./6., 9./8., 1./6., -1./96., 0. /)
        RHSMAT_B(1,:) =  (/-1./36., -14./18., 0., 14./18., 1./36. /)            
        RHSMAT_C(1,1:6)= -RHSMAT_A(2,6:1:-1) 
        RHSMAT_C(2,1:6)= -RHSMAT_A(1,6:1:-1)   
        		              
        !-------SOLVE THE RHS SYSTEM------------
        YI(1)= SUM(RHSMAT_A(1,1:6)*Y(1:6))
        YI(2)= SUM(RHSMAT_A(2,1:6)*Y(1:6))
                
        DO I=3,N-2
            YI(I) = SUM(RHSMAT_B(1,1:5)*Y(I:I+4))
        END DO

        YI(N-1)= SUM(RHSMAT_C(1,1:6)*Y(N-5:N))
        YI(N)= SUM(RHSMAT_C(2,1:6)*Y(N-5:N))

        CALL TDMA(N,LHSMAT(:,1),LHSMAT(:,2),LHSMAT(:,3),YI,YD)
  
    END SUBROUTINE DER_COMPACT_VEC
    
	!#######################################################################
    SUBROUTINE TDMA(N,A,B,C,D,X)
        USE MOD_PARAM
        !    a - sub-diagonal (means it is the diagonal below the main diagonal)
        !    b - the main diagonal
        !    c - sup-diagonal (means it is the diagonal above the main diagonal)
        !    d - right part
        !    x - the answer
        !    n - number of equations
        IMPLICIT NONE
        integer,intent(in) :: n
        real(KIND=DP),dimension(n),intent(in) :: a,b,c,d
        real(KIND=DP),dimension(n),intent(out) :: x
        real(KIND=DP),dimension(n) :: cpr,dpr
        real(KIND=DP) :: m
        integer 	  :: i

        ! initialize c-prime and d-prime
        cpr(1) = c(1)/b(1)
        dpr(1) = d(1)/b(1)
        ! solve for vectors c-prime and d-prime				
        do i = 2,n
            m = b(i)-cpr(i-1)*a(i)
            cpr(i) = c(i)/m
            dpr(i) = (d(i)-dpr(i-1)*a(i))/m
        enddo
        ! initialize x
         x(n) = dpr(n)
        ! solve for x from the vectors c-prime and d-prime
         do i = n-1, 1, -1
            x(i) = dpr(i)-cpr(i)*x(i+1)
        end do
        
    END SUBROUTINE TDMA
    
	!#######################################################################
END MODULE MOD_DERIVATIVE_COMPACT