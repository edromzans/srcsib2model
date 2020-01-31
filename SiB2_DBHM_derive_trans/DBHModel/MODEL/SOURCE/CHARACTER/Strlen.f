      subroutine strlen(str, l1,l2)
      character str*120
      integer i,l1,l2,k
      k=0
      do i = 1, 200
        if(k.eq.0 .and. str(i:i).NE.' ') then
          l1=i
          k=1
       elseif(k.eq.1.and. str(i:i).EQ.' '.or.ICHAR(str(i:i)).eq.0) then
          l2 = i-1
          return
        endif
      end do
      l2 = i
      return
      end

      subroutine strlen20(str, l1,l2)
      character str*20
      integer i,l1,l2,k
      k=0
      do i = 1, 200
        if(k.eq.0 .and. str(i:i).NE.' ') then
          l1=i
          k=1
        elseif(k.eq.1.and. str(i:i).EQ.' '.or.ICHAR(str(i:i)).eq.0) then
          l2 = i-1
          return
        endif
      end do
      l2 = i
      return
      end