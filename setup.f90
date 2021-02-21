MODULE setup

  USE shared_data
  USE control

  IMPLICIT NONE

CONTAINS

  SUBROUTINE after_control

    REAL(num) :: length

    ALLOCATE(xb(0:nx))
    ALLOCATE(yb(0:ny+1))
    ALLOCATE(zb(0:nz+1))
    ALLOCATE(xc(1:nx))
    ALLOCATE(yc(0:ny+1))
    ALLOCATE(zc(0:nz+1))

    ALLOCATE(     ux1(0:nx,0:ny+1,0:nz+1))
    ALLOCATE(    ux1m(0:nx,0:ny+1,0:nz+1))
    ALLOCATE(ux1_pred(0:nx,0:ny+1,0:nz+1))

    ALLOCATE(     ux2(0:nx,0:ny+1,0:nz+1))
    ALLOCATE(    ux2m(0:nx,0:ny+1,0:nz+1))
    ALLOCATE(ux2_pred(0:nx,0:ny+1,0:nz+1))

    ALLOCATE(     u_perp1(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(    u_perp1m(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(u_perp1_pred(1:nx,0:ny+1,0:nz+1))

    ALLOCATE(     u_perp2(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(    u_perp2m(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(u_perp2_pred(1:nx,0:ny+1,0:nz+1))

    ALLOCATE(     bx1(0:nx,0:ny+1,0:nz+1))
    ALLOCATE(    bx1m(0:nx,0:ny+1,0:nz+1))
    ALLOCATE(bx1_pred(0:nx,0:ny+1,0:nz+1))

    ALLOCATE(     bx2(0:nx,0:ny+1,0:nz+1))
    ALLOCATE(    bx2m(0:nx,0:ny+1,0:nz+1))
    ALLOCATE(bx2_pred(0:nx,0:ny+1,0:nz+1))

    ALLOCATE(     b_perp1(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(    b_perp1m(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(b_perp1_pred(1:nx,0:ny+1,0:nz+1))

    ALLOCATE(     b_perp2(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(    b_perp2m(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(b_perp2_pred(1:nx,0:ny+1,0:nz+1))

    ALLOCATE(     b_par1(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(    b_par1m(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(b_par1_pred(1:nx,0:ny+1,0:nz+1))

    ALLOCATE(     b_par2(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(    b_par2m(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(b_par2_pred(1:nx,0:ny+1,0:nz+1))

    ALLOCATE(rho_bcc(0:nx,0:ny+1,0:nz+1))
    ALLOCATE(rho_bbb(0:nx,0:ny+1,0:nz+1))
    ALLOCATE(rho_ccb(1:nx,0:ny+1,0:nz+1))
    ALLOCATE(rho_cbc(1:nx,0:ny+1,0:nz+1))

    ! Create grid
    dx = (x_max - x_min) / REAL(nx, num)
    dy = (y_max - y_min) / REAL(ny, num)
    dz = (z_max - z_min) / REAL(nz, num)
    DO ix = 0, nx
      xb(ix) = x_min + ix * dx
    END DO
    DO iy = 0, ny + 1
      yb(iy) = y_min + iy * dy
    END DO
    DO iz = 0, nz + 1
      zb(iz) = z_min + iz * dz
    END DO
    DO ix = 1, nx
      xc(ix) = x_min + (ix - 0.5_num) * dx
    END DO
    DO iy = 0, ny + 1
      yc(iy) = y_min + (iy - 0.5_num) * dy
    END DO
    DO iz = 0, nz + 1
      zc(iz) = z_min + (iz - 0.5_num) * dz
    END DO

    by0 = SIN(alpha)
    bz0 = COS(alpha)

    ! Denisty
    DO iz = 0, nz + 1
      DO iy = 0, ny + 1
        DO ix = 0, nx
                       rho_bcc(ix,iy,iz) = rho(xb(ix),yc(iy),zc(iz))
                       rho_bbb(ix,iy,iz) = rho(xb(ix),yb(iy),zb(iz))
          IF (ix /= 0) rho_ccb(ix,iy,iz) = rho(xc(ix),yc(iy),zb(iz))
          IF (ix /= 0) rho_cbc(ix,iy,iz) = rho(xc(ix),yb(iy),zc(iz))
        END DO
      END DO
    END DO

    ! CFL condition
    length = MIN(dx, dy, dz)
    dt = dt_multiplier * length * SQRT(MINVAL(rho_bbb))
    IF (n_iters < 0) n_iters = INT(t_max / (REAL(dt, num))) + 2

  END SUBROUTINE after_control

  SUBROUTINE finalize

    DEALLOCATE(xb)
    DEALLOCATE(yb)
    DEALLOCATE(zb)
    DEALLOCATE(xc)
    DEALLOCATE(yc)
    DEALLOCATE(zc)

    DEALLOCATE(     ux1)
    DEALLOCATE(    ux1m)
    DEALLOCATE(ux1_pred)

    DEALLOCATE(     ux2)
    DEALLOCATE(    ux2m)
    DEALLOCATE(ux2_pred)

    DEALLOCATE(     u_perp1)
    DEALLOCATE(    u_perp1m)
    DEALLOCATE(u_perp1_pred)

    DEALLOCATE(     u_perp2)
    DEALLOCATE(    u_perp2m)
    DEALLOCATE(u_perp2_pred)

    DEALLOCATE(     bx1)
    DEALLOCATE(    bx1m)
    DEALLOCATE(bx1_pred)

    DEALLOCATE(     bx2)
    DEALLOCATE(    bx2m)
    DEALLOCATE(bx2_pred)

    DEALLOCATE(     b_perp1)
    DEALLOCATE(    b_perp1m)
    DEALLOCATE(b_perp1_pred)

    DEALLOCATE(     b_perp2)
    DEALLOCATE(    b_perp2m)
    DEALLOCATE(b_perp2_pred)

    DEALLOCATE(     b_par1)
    DEALLOCATE(    b_par1m)
    DEALLOCATE(b_par1_pred)

    DEALLOCATE(     b_par2)
    DEALLOCATE(    b_par2m)
    DEALLOCATE(b_par2_pred)

    DEALLOCATE(rho_bcc)
    DEALLOCATE(rho_bbb)
    DEALLOCATE(rho_ccb)
    DEALLOCATE(rho_cbc)

  END SUBROUTINE finalize

END MODULE setup