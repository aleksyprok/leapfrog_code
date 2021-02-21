MODULE control

  USE shared_data
  USE constants

  IMPLICIT NONE

CONTAINS

  SUBROUTINE control_variables

    alpha = 0.0_num

    nx = 32
    ny = 1
    nz = 32
    x_min = -0.5_num
    x_max =  0.5_num
    y_min = -0.5_num
    y_max =  0.5_num
    z_min = -0.5_num
    z_max =  0.5_num
    t_max = 10.0_num
    n_iters = -1

    output_ux1     = .TRUE.
    output_ux2     = .TRUE.
    output_u_perp1 = .TRUE.
    output_u_perp2 = .TRUE.
    output_bx1     = .TRUE.
    output_bx2     = .TRUE.
    output_b_perp1 = .TRUE.
    output_b_perp2 = .TRUE.
    output_b_par1  = .TRUE.
    output_b_par2  = .TRUE.

    dt_snapshots = 0.1_num

    dt_multiplier = 0.4_num

  END SUBROUTINE control_variables

  SUBROUTINE set_initial_conditions

    REAL(num) :: Lz, kz

    ux1  = 0.0_num
    ux1m = 0.0_num
    ux2  = 0.0_num
    ux2m = 0.0_num
    u_perp1  = 0.0_num
    u_perp1m = 0.0_num
    u_perp2  = 0.0_num
    u_perp2m = 0.0_num
    bx1  = 0.0_num
    bx1m = 0.0_num
    bx2  = 0.0_num
    bx2m = 0.0_num
    b_perp1  = 0.0_num
    b_perp1m = 0.0_num
    b_perp2  = 0.0_num
    b_perp2m = 0.0_num
    b_par1  = 0.0_num
    b_par1m = 0.0_num
    b_par2  = 0.0_num
    b_par2m = 0.0_num

    Lz = z_max - z_min
    kz = 2.0_num * pi / Lz

    DO iz = 0, nz + 1
      DO iy = 0, ny + 1
        DO ix = 1, nx
          u_perp1( ix,iy,iz) = DSIN(kz * zb(iz))
          u_perp1m(ix,iy,iz) = DSIN(kz * zb(iz))
          u_perp2( ix,iy,iz) = DSIN(kz * zc(iz))
          u_perp2m(ix,iy,iz) = DSIN(kz * zc(iz))
        END DO
      END DO
    END DO

  END SUBROUTINE set_initial_conditions

  FUNCTION rho(x,y,z)

    REAL(num), INTENT(IN) :: x, y, z
    REAL(num) :: rho

    rho = 1.0_num

  END FUNCTION rho

  SUBROUTINE set_boundary_conditions(variables, time)

    INTEGER, INTENT(IN) :: variables
    REAL(num), INTENT(IN) :: time

    IF (variables == CURR_VARIABLES) THEN
      CALL boundary_conditions(ux1, ux2, u_perp1, u_perp2, bx1, bx2, &
                               b_perp1, b_perp2, b_par1, b_par2, time)
    ELSE IF (variables == PRED_VARIABLES) THEN
      CALL boundary_conditions(ux1_pred, ux2_pred, u_perp1_pred, u_perp2_pred, &
                               bx1_pred, bx2_pred, b_perp1_pred, b_perp2_pred, &
                               b_par1_pred, b_par2_pred, time)
    ELSE IF (variables == M_VARIABLES) THEN
      CALL boundary_conditions(ux1m, ux2m, u_perp1m, u_perp2m, bx1m, bx2m, &
                               b_perp1m, b_perp2m, b_par1m, b_par2m, time)
    END IF

  END SUBROUTINE set_boundary_conditions

  SUBROUTINE boundary_conditions(ux1, ux2, u_perp1, u_perp2, bx1, bx2, &
                                 b_perp1, b_perp2, b_par1, b_par2, time)

    REAL(num), DIMENSION(0:,0:,0:), INTENT(INOUT) :: ux1, ux2, bx1, bx2
    REAL(num), DIMENSION(1:,0:,0:), INTENT(INOUT) :: u_perp1, u_perp2, b_perp1, b_perp2
    REAL(num), DIMENSION(1:,0:,0:), INTENT(INOUT) :: b_par1, b_par2
    REAL(num), INTENT(IN) :: time

    ! x = x_min
    ux1(0,:,:) = 0.0_num
    ux2(0,:,:) = 0.0_num
    bx1(0,:,:) = 0.0_num
    bx2(0,:,:) = 0.0_num

    ! x = x_max
    ux1(nx,:,:) = 0.0_num
    ux2(nx,:,:) = 0.0_num
    bx1(nx,:,:) = 0.0_num
    bx2(nx,:,:) = 0.0_num

    ! y = y_min
    ux1(:,0,:) = ux1(:,ny,:)
    ux2(:,0,:) = ux2(:,ny,:)
    u_perp1(:,0,:) = u_perp1(:,ny,:)
    u_perp2(:,0,:) = u_perp2(:,ny,:)
    bx1(:,0,:) = bx1(:,ny,:)
    bx2(:,0,:) = bx2(:,ny,:)
    b_perp1(:,0,:) = b_perp1(:,ny,:)
    b_perp2(:,0,:) = b_perp2(:,ny,:)
    b_par1(:,0,:) = b_par1(:,ny,:)
    b_par2(:,0,:) = b_par2(:,ny,:)

    ! y = y_max
    ux1(:,ny+1,:) = ux1(:,1,:)
    ux2(:,ny+1,:) = ux2(:,1,:)
    u_perp1(:,ny+1,:) = u_perp1(:,1,:)
    u_perp2(:,ny+1,:) = u_perp2(:,1,:)
    bx1(:,ny+1,:) = bx1(:,1,:)
    bx2(:,ny+1,:) = bx2(:,1,:)
    b_perp1(:,ny+1,:) = b_perp1(:,1,:)
    b_perp2(:,ny+1,:) = b_perp2(:,1,:)
    b_par1(:,ny+1,:) = b_par1(:,1,:)
    b_par2(:,ny+1,:) = b_par2(:,1,:)

    ! z = z_min
    ux1(:,:,0) = ux1(:,:,nz)
    ux2(:,:,0) = ux2(:,:,nz)
    u_perp1(:,:,0) = u_perp1(:,:,nz)
    u_perp2(:,:,0) = u_perp2(:,:,nz)
    bx1(:,:,0) = bx1(:,:,nz)
    bx2(:,:,0) = bx2(:,:,nz)
    b_perp1(:,:,0) = b_perp1(:,:,nz)
    b_perp2(:,:,0) = b_perp2(:,:,nz)
    b_par1(:,:,0) = b_par1(:,:,nz)
    b_par2(:,:,0) = b_par2(:,:,nz)

    ! z = z_max
    ux1(:,:,nz+1) = ux1(:,:,1)
    ux2(:,:,nz+1) = ux2(:,:,1)
    u_perp1(:,:,nz+1) = u_perp1(:,:,1)
    u_perp2(:,:,nz+1) = u_perp2(:,:,1)
    bx1(:,:,nz+1) = bx1(:,:,1)
    bx2(:,:,nz+1) = bx2(:,:,1)
    b_perp1(:,:,nz+1) = b_perp1(:,:,1)
    b_perp2(:,:,nz+1) = b_perp2(:,:,1)
    b_par1(:,:,nz+1) = b_par1(:,:,1)
    b_par2(:,:,nz+1) = b_par2(:,:,1)

  END SUBROUTINE boundary_conditions

  SUBROUTINE backward_step
  ! This subroutine can be used to approximate the solution at the previous time step,
  ! which can be useful in set_initial_conditions as the code needs to be initialised at
  ! two consecutive time steps.
  
    REAL(num) :: w1, w2, w3, w4, wa

    DO iz = 1, nz
      izp = iz + 1
      izm = iz - 1
      DO iy = 1, ny
        iyp = iy + 1
        iym = iy - 1
        DO ix = 0, nx
          ixp = ix + 1
          ixm = ix - 1

          IF (ix /= 0 .AND. ix /= nx) THEN
            w1 = by0 * (bx2(ix,iy,iz) - bx2(ix,iym,iz)) / dy
            w2 = bz0 * (bx1(ix,iy,iz) - bx1(ix,iy,izm)) / dz
            w3 = (b_par1(ixp,iy,iz) - b_par1(ix,iy,iz)) / dx
            wa = (w1 + w2 - w3) / rho_bcc(ix,iy,iz)
            ux1m(ix,iy,iz) = ux1(ix,iy,iz) - dt * wa

            w1 = by0 * (bx1(ix,iyp,iz) - bx1(ix,iy,iz)) / dy
            w2 = bz0 * (bx2(ix,iy,izp) - bx2(ix,iy,iz)) / dz
            w3 = (b_par2(ixp,iy,iz) - b_par2(ix,iy,iz)) / dx
            wa = (w1 + w2 - w3) / rho_bbb(ix,iy,iz)
            ux2m(ix,iy,iz) = ux2(ix,iy,iz) - dt * wa
          END IF

          w1 = by0 * (ux2(ix,iy,iz) - ux2(ix,iym,iz)) / dy
          w2 = bz0 * (ux1(ix,iy,izp) - ux1(ix,iy,iz)) / dz
          wa = w1 + w2
          bx1m(ix,iy,iz) = bx1(ix,iy,iz) - dt * wa

          w1 = by0 * (ux1(ix,iyp,iz) - ux1(ix,iy,iz)) / dy
          w2 = bz0 * (ux2(ix,iy,iz) - ux2(ix,iy,izm)) / dz
          wa = w1 + w2
          bx2m(ix,iy,iz) = bx2(ix,iy,iz) - dt * wa

          IF (ix /= 0) THEN
            w1 = by0 * (b_perp2(ix,iy,iz) - b_perp2(ix,iym,iz)) / dy
            w2 = bz0 * (b_perp1(ix,iy,izp) - b_perp1(ix,iy,iz)) / dz
            w3 = bz0 * (b_par2(ix,iy,iz) - b_par2(ix,iym,iz)) / dy
            w4 = by0 * (b_par1(ix,iy,izp) - b_par1(ix,iy,iz)) / dz
            wa = (w1 + w2 - w3 + w4) / rho_ccb(ix,iy,iz)
            u_perp1m(ix,iy,iz) = u_perp1(ix,iy,iz) - dt * wa

            w1 = by0 * (b_perp1(ix,iyp,iz) - b_perp1(ix,iy,iz)) / dy
            w2 = bz0 * (b_perp2(ix,iy,iz) - b_perp2(ix,iy,izm)) / dz
            w3 = bz0 * (b_par1(ix,iyp,iz) - b_par1(ix,iy,iz)) / dy
            w4 = by0 * (b_par2(ix,iy,iz) - b_par2(ix,iy,izm)) / dz
            wa = (w1 + w2 - w3 + w4) / rho_cbc(ix,iy,iz)
            u_perp2m(ix,iy,iz) = u_perp2(ix,iy,iz) - dt * wa

            w1 = by0 * (u_perp2(ix,iy,iz) - u_perp2(ix,iym,iz)) / dy
            w2 = bz0 * (u_perp1(ix,iy,iz) - u_perp1(ix,iy,izm)) / dz
            wa = w1 + w2
            b_perp1m(ix,iy,iz) = b_perp1(ix,iy,iz) - dt * wa

            w1 = by0 * (u_perp1(ix,iyp,iz) - u_perp1(ix,iy,iz)) / dy
            w2 = bz0 * (u_perp2(ix,iy,izp) - u_perp2(ix,iy,iz)) / dz
            wa = w1 + w2
            b_perp2m(ix,iy,iz) = b_perp2(ix,iy,iz) - dt * wa

            w1 = (ux1(ix,iy,iz) - ux1(ixm,iy,iz)) / dx
            w2 = bz0 * (u_perp2(ix,iy,iz) - u_perp2(ix,iym,iz)) / dy
            w3 = by0 * (u_perp1(ix,iy,iz) - u_perp1(ix,iy,izm)) / dz
            wa = w1 + w2 - w3
            b_par1m(ix,iy,iz) = b_par1(ix,iy,iz) - dt * wa

            w1 = (ux2_pred(ix,iy,iz) - ux2_pred(ixm,iy,iz)) / dx
            w2 = bz0 * (u_perp1_pred(ix,iyp,iz) - u_perp1_pred(ix,iy,iz)) / dy
            w3 = by0 * (u_perp2_pred(ix,iy,izp) - u_perp2_pred(ix,iy,iz)) / dz
            wa = w1 + w2 - w3
          END IF

        END DO
      END DO
    END DO

  END SUBROUTINE backward_step

END MODULE control