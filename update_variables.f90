MODULE update_variables

  USE shared_data
  USE control

  IMPLICIT NONE

CONTAINS

  SUBROUTINE predictor_corrector

    REAL(num) :: w1, w2, w3, w4, wa

    ! Predictor step
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
            ux1_pred(ix,iy,iz) = ux1m(ix,iy,iz) + 2.0_num * dt * wa

            w1 = by0 * (bx1(ix,iyp,iz) - bx1(ix,iy,iz)) / dy
            w2 = bz0 * (bx2(ix,iy,izp) - bx2(ix,iy,iz)) / dz
            w3 = (b_par2(ixp,iy,iz) - b_par2(ix,iy,iz)) / dx
            wa = (w1 + w2 - w3) / rho_bbb(ix,iy,iz)
            ux2_pred(ix,iy,iz) = ux2m(ix,iy,iz) + 2.0_num * dt * wa
          END IF

          w1 = by0 * (ux2(ix,iy,iz) - ux2(ix,iym,iz)) / dy
          w2 = bz0 * (ux1(ix,iy,izp) - ux1(ix,iy,iz)) / dz
          bx1_pred(ix,iy,iz) = bx1m(ix,iy,iz) + 2.0_num * dt * (w1 + w2)

          w1 = by0 * (ux1(ix,iyp,iz) - ux1(ix,iy,iz)) / dy
          w2 = bz0 * (ux2(ix,iy,iz) - ux2(ix,iy,izm)) / dz
          bx2_pred(ix,iy,iz) = bx2m(ix,iy,iz) + 2.0_num * dt * (w1 + w2)

          IF (ix /= 0) THEN
            w1 = by0 * (b_perp2(ix,iy,iz) - b_perp2(ix,iym,iz)) / dy
            w2 = bz0 * (b_perp1(ix,iy,izp) - b_perp1(ix,iy,iz)) / dz
            w3 = bz0 * (b_par2(ix,iy,iz) - b_par2(ix,iym,iz)) / dy
            w4 = by0 * (b_par1(ix,iy,izp) - b_par1(ix,iy,iz)) / dz
            wa = (w1 + w2 - w3 + w4) / rho_ccb(ix,iy,iz)
            u_perp1_pred(ix,iy,iz) = u_perp1m(ix,iy,iz) + 2.0_num * dt * wa

            w1 = by0 * (b_perp1(ix,iyp,iz) - b_perp1(ix,iy,iz)) / dy
            w2 = bz0 * (b_perp2(ix,iy,iz) - b_perp2(ix,iy,izm)) / dz
            w3 = bz0 * (b_par1(ix,iyp,iz) - b_par1(ix,iy,iz)) / dy
            w4 = by0 * (b_par2(ix,iy,iz) - b_par2(ix,iy,izm)) / dz
            wa = (w1 + w2 - w3 + w4) / rho_cbc(ix,iy,iz)
            u_perp2_pred(ix,iy,iz) = u_perp2m(ix,iy,iz) + 2.0_num * dt * wa

            w1 = by0 * (u_perp2(ix,iy,iz) - u_perp2(ix,iym,iz)) / dy
            w2 = bz0 * (u_perp1(ix,iy,iz) - u_perp1(ix,iy,izm)) / dz
            b_perp1_pred(ix,iy,iz) = b_perp1m(ix,iy,iz) + 2.0_num * dt * (w1 + w2)

            w1 = by0 * (u_perp1(ix,iyp,iz) - u_perp1(ix,iy,iz)) / dy
            w2 = bz0 * (u_perp2(ix,iy,izp) - u_perp2(ix,iy,iz)) / dz
            b_perp2_pred(ix,iy,iz) = b_perp2m(ix,iy,iz) + 2.0_num * dt * (w1 + w2)

            w1 = (ux1(ix,iy,iz) - ux1(ixm,iy,iz)) / dx
            w2 = bz0 * (u_perp2(ix,iy,iz) - u_perp2(ix,iym,iz)) / dy
            w3 = by0 * (u_perp1(ix,iy,iz) - u_perp1(ix,iy,izm)) / dz
            b_par1_pred(ix,iy,iz) = b_par1m(ix,iy,iz) - 2.0_num * dt * (w1 + w2 - w3)

            w1 = (ux2(ix,iy,iz) - ux2(ixm,iy,iz)) / dx
            w2 = bz0 * (u_perp1(ix,iyp,iz) - u_perp1(ix,iy,iz)) / dy
            w3 = by0 * (u_perp2(ix,iy,izp) - u_perp2(ix,iy,iz)) / dz
            b_par2_pred(ix,iy,iz) = b_par2m(ix,iy,iz) - 2.0_num * dt * (w1 + w2 - w3)
          END IF

        END DO
      END DO
    END DO

    CALL set_boundary_conditions(PRED_VARIABLES, time + dt)

    ! Overwrite m variables
    ux1m = ux1
    ux2m = ux2
    u_perp1m = u_perp1
    u_perp2m = u_perp2
    bx1m = bx1
    bx2m = bx2
    b_perp1m = b_perp1
    b_perp2m = b_perp2
    b_par1m = b_par1
    b_par2m = b_par2

    ! Corrector step
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
            w1 = by0 * (bx2m(ix,iy,iz) - bx2m(ix,iym,iz)) / dy
            w2 = bz0 * (bx1m(ix,iy,iz) - bx1m(ix,iy,izm)) / dz
            w3 = (b_par1m(ixp,iy,iz) - b_par1m(ix,iy,iz)) / dx
            wa = w1 + w2 - w3
            w1 = by0 * (bx2_pred(ix,iy,iz) - bx2_pred(ix,iym,iz)) / dy
            w2 = bz0 * (bx1_pred(ix,iy,iz) - bx1_pred(ix,iy,izm)) / dz
            w3 = (b_par1_pred(ixp,iy,iz) - b_par1_pred(ix,iy,iz)) / dx
            wa = 0.5_num * (wa + w1 + w2 - w3) / rho_bcc(ix,iy,iz)
            ux1(ix,iy,iz) = ux1(ix,iy,iz) + dt * wa

            w1 = by0 * (bx1m(ix,iyp,iz) - bx1m(ix,iy,iz)) / dy
            w2 = bz0 * (bx2m(ix,iy,izp) - bx2m(ix,iy,iz)) / dz
            w3 = (b_par2m(ixp,iy,iz) - b_par2m(ix,iy,iz)) / dx
            wa = w1 + w2 - w3
            w1 = by0 * (bx1_pred(ix,iyp,iz) - bx1_pred(ix,iy,iz)) / dy
            w2 = bz0 * (bx2_pred(ix,iy,izp) - bx2_pred(ix,iy,iz)) / dz
            w3 = (b_par2_pred(ixp,iy,iz) - b_par2_pred(ix,iy,iz)) / dx
            wa = 0.5_num * (wa + w1 + w2 - w3) / rho_bbb(ix,iy,iz)
            ux2(ix,iy,iz) = ux2(ix,iy,iz) + dt * wa
          END IF

          w1 = by0 * (ux2m(ix,iy,iz) - ux2m(ix,iym,iz)) / dy
          w2 = bz0 * (ux1m(ix,iy,izp) - ux1m(ix,iy,iz)) / dz
          wa = w1 + w2
          w1 = by0 * (ux2_pred(ix,iy,iz) - ux2_pred(ix,iym,iz)) / dy
          w2 = bz0 * (ux1_pred(ix,iy,izp) - ux1_pred(ix,iy,iz)) / dz
          wa = 0.5_num * (wa + w1 + w2)
          bx1(ix,iy,iz) = bx1(ix,iy,iz) + dt * wa

          w1 = by0 * (ux1m(ix,iyp,iz) - ux1m(ix,iy,iz)) / dy
          w2 = bz0 * (ux2m(ix,iy,iz) - ux2m(ix,iy,izm)) / dz
          wa = w1 + w2
          w1 = by0 * (ux1_pred(ix,iyp,iz) - ux1_pred(ix,iy,iz)) / dy
          w2 = bz0 * (ux2_pred(ix,iy,iz) - ux2_pred(ix,iy,izm)) / dz
          wa = 0.5_num * (wa + w1 + w2)
          bx2(ix,iy,iz) = bx2(ix,iy,iz) + dt * wa

          IF (ix /= 0) THEN
            w1 = by0 * (b_perp2m(ix,iy,iz) - b_perp2m(ix,iym,iz)) / dy
            w2 = bz0 * (b_perp1m(ix,iy,izp) - b_perp1m(ix,iy,iz)) / dz
            w3 = bz0 * (b_par2m(ix,iy,iz) - b_par2m(ix,iym,iz)) / dy
            w4 = by0 * (b_par1m(ix,iy,izp) - b_par1m(ix,iy,iz)) / dz
            wa = w1 + w2 - w3 + w4
            w1 = by0 * (b_perp2_pred(ix,iy,iz) - b_perp2_pred(ix,iym,iz)) / dy
            w2 = bz0 * (b_perp1_pred(ix,iy,izp) - b_perp1_pred(ix,iy,iz)) / dz
            w3 = bz0 * (b_par2_pred(ix,iy,iz) - b_par2_pred(ix,iym,iz)) / dy
            w4 = by0 * (b_par1_pred(ix,iy,izp) - b_par1_pred(ix,iy,iz)) / dz
            wa = 0.5_num * (wa + w1 + w2 - w3 + w4) / rho_ccb(ix,iy,iz)
            u_perp1(ix,iy,iz) = u_perp1(ix,iy,iz) + dt * wa

            w1 = by0 * (b_perp1m(ix,iyp,iz) - b_perp1m(ix,iy,iz)) / dy
            w2 = bz0 * (b_perp2m(ix,iy,iz) - b_perp2m(ix,iy,izm)) / dz
            w3 = bz0 * (b_par1m(ix,iyp,iz) - b_par1m(ix,iy,iz)) / dy
            w4 = by0 * (b_par2m(ix,iy,iz) - b_par2m(ix,iy,izm)) / dz
            wa = w1 + w2 - w3 + w4
            w1 = by0 * (b_perp1_pred(ix,iyp,iz) - b_perp1_pred(ix,iy,iz)) / dy
            w2 = bz0 * (b_perp2_pred(ix,iy,iz) - b_perp2_pred(ix,iy,izm)) / dz
            w3 = bz0 * (b_par1_pred(ix,iyp,iz) - b_par1_pred(ix,iy,iz)) / dy
            w4 = by0 * (b_par2_pred(ix,iy,iz) - b_par2_pred(ix,iy,izm)) / dz
            wa = 0.5_num * (wa + w1 + w2 - w3 + w4) / rho_cbc(ix,iy,iz)
            u_perp2(ix,iy,iz) = u_perp2(ix,iy,iz) + dt * wa

            w1 = by0 * (u_perp2m(ix,iy,iz) - u_perp2m(ix,iym,iz)) / dy
            w2 = bz0 * (u_perp1m(ix,iy,iz) - u_perp1m(ix,iy,izm)) / dz
            wa = w1 + w2
            w1 = by0 * (u_perp2_pred(ix,iy,iz) - u_perp2_pred(ix,iym,iz)) / dy
            w2 = bz0 * (u_perp1_pred(ix,iy,iz) - u_perp1_pred(ix,iy,izm)) / dz
            wa = 0.5_num * (wa + w1 + w2)
            b_perp1(ix,iy,iz) = b_perp1(ix,iy,iz) + dt * wa

            w1 = by0 * (u_perp1m(ix,iyp,iz) - u_perp1m(ix,iy,iz)) / dy
            w2 = bz0 * (u_perp2m(ix,iy,izp) - u_perp2m(ix,iy,iz)) / dz
            wa = w1 + w2
            w1 = by0 * (u_perp1_pred(ix,iyp,iz) - u_perp1_pred(ix,iy,iz)) / dy
            w2 = bz0 * (u_perp2_pred(ix,iy,izp) - u_perp2_pred(ix,iy,iz)) / dz
            wa = 0.5_num * (wa + w1 + w2)
            b_perp2(ix,iy,iz) = b_perp2(ix,iy,iz) + dt * wa

            w1 = (ux1m(ix,iy,iz) - ux1m(ixm,iy,iz)) / dx
            w2 = bz0 * (u_perp2m(ix,iy,iz) - u_perp2m(ix,iym,iz)) / dy
            w3 = by0 * (u_perp1m(ix,iy,iz) - u_perp1m(ix,iy,izm)) / dz
            wa = -(w1 + w2 - w3)
            w1 = (ux1_pred(ix,iy,iz) - ux1_pred(ixm,iy,iz)) / dx
            w2 = bz0 * (u_perp2_pred(ix,iy,iz) - u_perp2_pred(ix,iym,iz)) / dy
            w3 = by0 * (u_perp1_pred(ix,iy,iz) - u_perp1_pred(ix,iy,izm)) / dz
            wa = 0.5_num * (wa - (w1 + w2 - w3))
            b_par1(ix,iy,iz) = b_par1(ix,iy,iz) + dt * wa

            w1 = (ux2m(ix,iy,iz) - ux2m(ixm,iy,iz)) / dx
            w2 = bz0 * (u_perp1m(ix,iyp,iz) - u_perp1m(ix,iy,iz)) / dy
            w3 = by0 * (u_perp2m(ix,iy,izp) - u_perp2m(ix,iy,iz)) / dz
            wa = -(w1 + w2 - w3)
            w1 = (ux2_pred(ix,iy,iz) - ux2_pred(ixm,iy,iz)) / dx
            w2 = bz0 * (u_perp1_pred(ix,iyp,iz) - u_perp1_pred(ix,iy,iz)) / dy
            w3 = by0 * (u_perp2_pred(ix,iy,izp) - u_perp2_pred(ix,iy,iz)) / dz
            wa = 0.5_num * (wa - (w1 + w2 - w3))
            b_par2(ix,iy,iz) = b_par2(ix,iy,iz) + dt * wa
          END IF

        END DO
      END DO
    END DO

    CALL set_boundary_conditions(CURR_VARIABLES, time + dt)

  END SUBROUTINE predictor_corrector

END MODULE update_variables