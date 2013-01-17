program fpop
  implicit none
  integer narg,iargc,ios,ikind,iopt,iret,npos
  character(80) cop,cx,cy,ca,copt,carg
  character(16) cform
  real(4) x_4,y_4,a_4
  real(8) x_8,y_8,a_8
  real(16) x_16,y_16,a_16
  cform=' '
  ikind=8
  do 
    call getopts('f:k:',copt,carg,iopt,iret)
    if(iret.ne.0) exit
    select case(copt)
    case('f')
      select case(carg)
      case('I','int')
        cform='I'
      case('D','dec')
        cform='D'
      case('B','bin')
        cform='B'
      case('O','oct')
        cform='O'
      case('Z','hex')
        cform='Z'
      case('z')
        cform='z'
      case('(  ':'(zz')
        cform=carg
      case default
        call errmsg('fpop: invalid form '//trim(carg))
        call errexit(1)
      end select
    case('k')
      select case(carg)
      case('4','S','single','+4')
        ikind=4
      case('8','D','double','+8')
        ikind=8
      case('16','+16')
        ikind=16
      case default
        call errmsg('fpop: invalid kind '//trim(carg))
        call errexit(1)
      end select
    case('?',':')
      call errmsg('fpop: invalid option '//carg(1:1))
      call errexit(1)
    case default
      call errmsg('fpop: invalid option '//copt(1:1))
      call errexit(1)
    end select
  enddo
  narg=iargc()
  npos=narg-iopt+1
  select case(ikind)
  case(4)
    select case(npos)
    case(1)
      call getarg(iopt,cop)
      select case(index('abcdefghijklmnopqrstuvwxy',cop(1:1)))
      case(0)
        a_4=fpop_read_4(cop)
      case default
        a_4=fpop_query_4(cop)
      end select
    case(2)
      call getarg(iopt,cop)
      call getarg(iopt+1,cx)
      x_4=fpop_read_4(cx)
      a_4=fpop_unary_4(cop,x_4)
    case(3)
      call getarg(iopt,cx)
      x_4=fpop_read_4(cx)
      call getarg(iopt+1,cop)
      call getarg(iopt+2,cy)
      y_4=fpop_read_4(cy)
      a_4=fpop_binary_4(x_4,cop,y_4)
    case default
      call eusage
      call errexit(4)
    end select
    ca=fpop_write_4(a_4,cform)
  case(8)
    select case(npos)
    case(1)
      call getarg(iopt,cop)
      select case(index('abcdefghijklmnopqrstuvwxy',cop(1:1)))
      case(0)
        a_8=fpop_read_8(cop)
      case default
        a_8=fpop_query_8(cop)
      end select
    case(2)
      call getarg(iopt,cop)
      call getarg(iopt+1,cx)
      x_8=fpop_read_8(cx)
      a_8=fpop_unary_8(cop,x_8)
    case(3)
      call getarg(iopt,cx)
      x_8=fpop_read_8(cx)
      call getarg(iopt+1,cop)
      call getarg(iopt+2,cy)
      y_8=fpop_read_8(cy)
      a_8=fpop_binary_8(x_8,cop,y_8)
    case default
      call eusage
      call errexit(4)
    end select
    ca=fpop_write_8(a_8,cform)
  case(16)
    select case(npos)
    case(1)
      call getarg(iopt,cop)
      select case(index('abcdefghijklmnopqrstuvwxy',cop(1:1)))
      case(0)
        a_16=fpop_read_16(cop)
      case default
        a_16=fpop_query_16(cop)
      end select
    case(2)
      call getarg(iopt,cop)
      call getarg(iopt+1,cx)
      x_16=fpop_read_16(cx)
      a_16=fpop_unary_16(cop,x_16)
    case(3)
      call getarg(iopt,cx)
      x_16=fpop_read_16(cx)
      call getarg(iopt+1,cop)
      call getarg(iopt+2,cy)
      y_16=fpop_read_16(cy)
      a_16=fpop_binary_16(x_16,cop,y_16)
    case default
      call eusage
      call errexit(4)
    end select
    ca=fpop_write_16(a_16,cform)
  end select
  print '(a)',trim(ca)
contains
  subroutine eusage
    implicit none
    call errmsg('Usage: fpop [-f form] [-k kind] &
                             &{ x | query_op | unary_op x | x binary_op y }')
    call errmsg('       Choose form from:')
    call errmsg('         I,int,D,dec,B,bin,O,oct,Z,hex,z,(format)')
    call errmsg('       Choose kind from:')
    call errmsg('         4,S,single,8,D,double,16')
    call errmsg('         (default is 8; kind 16 has limited functionality)')
    call errmsg('       Choose query_op from:')
    call errmsg('         kind,radix,digits,minexponent,maxexponent')
    call errmsg('         precision,range,epsilon,tiny,huge')
    call errmsg('       Choose unary_op from:')
    call errmsg('         -,minus,abs,int,nint,floor,ceiling')
    call errmsg('         exponent,fraction,rrspacing,spacing,nearest+,nearest-')
    call errmsg('         sqrt,exp,log,log10,sinh,cosh,tanh')
    call errmsg('         sin,cos,tan,asin,acos,atan,degree,radian')
    call errmsg('       Choose binary_op from:')
    call errmsg('         +,plus,-,minus,*,x,times,/,over,**,^,power')
    call errmsg('         min,max,mod,modulo,atan2')
    call errmsg('         round,round+,round-')
    call errmsg('       Choose floating point numbers x and y from the format:')
    call errmsg('         [D|B|O|Z][+|-][#][.][#][E{+|-}#] or z#')
    call errmsg('         where # is a legal number in the given base')
  end subroutine
  function fpop_query_4(cop) result(a)
    implicit none
    character*80,intent(in):: cop
    real(4):: a
    select case(cop)
    case('kind')
      a=kind(a)
    case('radix')
      a=radix(a)
    case('digits')
      a=digits(a)
    case('minexponent')
      a=minexponent(a)
    case('maxexponent')
      a=maxexponent(a)
    case('precision')
      a=precision(a)
    case('range')
      a=range(a)
    case('epsilon')
      a=epsilon(a)
    case('tiny')
      a=tiny(a)
    case('huge')
      a=huge(a)
    case default
      call errmsg('fpop: unrecognized query operation '//trim(cop))
      call errexit(2)
    end select
  end function
  function fpop_unary_4(cop,x) result(a)
    implicit none
    character*80,intent(in):: cop
    real(4),intent(in):: x
    real(4):: a
    select case(cop)
    case('-','minus')
      a=-x
    case('abs')
      a=abs(x)
    case('int')
      a=int(x,4)
    case('nint')
      a=nint(x,4)
    case('floor')
      a=floor(x,4)
    case('ceiling')
      a=ceiling(x,4)
    case('exponent')
      a=exponent(x)
    case('fraction')
      a=fraction(x)
    case('rrspacing')
      a=rrspacing(x)
    case('spacing')
      a=spacing(x)
    case('nearest+')
      a=nearest(x,+1._4)
    case('nearest-')
      a=nearest(x,-1._4)
    case('sqrt')
      a=sqrt(x)
    case('exp')
      a=exp(x)
    case('log')
      a=log(x)
    case('log10')
      a=log10(x)
    case('sinh')
      a=sinh(x)
    case('cosh')
      a=cosh(x)
    case('tanh')
      a=tanh(x)
    case('sin')
      a=sin(x)
    case('cos')
      a=cos(x)
    case('tan')
      a=tan(x)
    case('asin')
      a=asin(x)
    case('acos')
      a=acos(x)
    case('atan')
      a=atan(x)
    case('degree')
      a=x*180/acos(-1._4)
    case('radian')
      a=x*acos(-1._4)/180
    case('erf')
      a=erf(x)
    case('gamma')
      a=gamma(x)
    case default
      call errmsg('fpop: unrecognized unary operation '//trim(cop))
      call errexit(2)
    end select
  end function
  function fpop_binary_4(x,cop,y) result(a)
    implicit none
    character*80,intent(in):: cop
    real(4),intent(in):: x,y
    real(4):: a
    select case(cop)
    case('+','plus')
      a=x+y
    case('-','minus')
      a=x-y
    case('*','x','times')
      a=x*y
    case('/','over')
      a=x/y
    case('**','^','power')
      a=x**y
    case('min')
      a=min(x,y)
    case('max')
      a=max(x,y)
    case('mod')
      a=mod(x,y)
    case('modulo')
      a=modulo(x,y)
    case('atan2')
      a=atan2(x,y)
    case('round')
      a=nint(x/y,4)*y
    case('round+')
      a=ceiling(x/y,4)*y
    case('round-')
      a=floor(x/y,4)*y
    case default
      call errmsg('fpop: unrecognized binary operation '//trim(cop))
      call errexit(2)
    end select
  end function
  function fpop_read_4(cx) result(x)
    implicit none
    character*(*) cx
    real(4) x
    integer(4) nl,nr,ne
    integer lc,lp,le,lb,ns,np,nt,ios
    character(16) cf
    lc=len_trim(cx)
    select case(cx(1:1))
    case('D')
      read(cx(2:lc),*,iostat=ios) x
    case('B')
      le=index(cx(2:),'E')
      if(le.eq.0) le=lc
      lp=index(cx(2:le),'.')
      if(lp.eq.0) lp=le
      lb=0
      ns=1
      if(cx(2:2).eq.'+') then
        lb=1
      elseif(cx(2:2).eq.'-') then
        lb=1
        ns=-1
      endif
      nl=0
      if(lp.ge.lb+2) then
        write(cf,'("(B",i2.2,")")') lp-lb-1
        read(cx(lb+2:lp),cf,iostat=ios) nl
      endif
      nr=0
      np=0
      if(le.ge.lp+2) then
        write(cf,'("(B",i2.2,")")') le-lp-1
        read(cx(lp+2:le),cf,iostat=ios) nr
        np=le-lp-1
      endif
      nt=1
      if(lc.ge.le+2.and.cx(le+2:le+2).eq.'-') then
        le=le+1
        nt=-1
      elseif(lc.ge.le+2.and.cx(le+2:le+2).eq.'+') then
        le=le+1
        nt=+1
      endif
      ne=0
      if(lc.ge.le+2) then
        write(cf,'("(B",i2.2,")")') lc-le-1
        read(cx(le+2:lc),cf,iostat=ios) ne
      endif
      x=ns*(nl+nr*2._4**(-np))*2._4**(nt*ne)
    case('O')
      le=index(cx(2:),'E')
      if(le.eq.0) le=lc
      lp=index(cx(2:le),'.')
      if(lp.eq.0) lp=le
      lb=0
      ns=1
      if(cx(2:2).eq.'+') then
        lb=1
      elseif(cx(2:2).eq.'-') then
        lb=1
        ns=-1
      endif
      nl=0
      if(lp.ge.lb+2) then
        write(cf,'("(O",i2.2,")")') lp-lb-1
        read(cx(lb+2:lp),cf,iostat=ios) nl
      endif
      nr=0
      np=0
      if(le.ge.lp+2) then
        write(cf,'("(O",i2.2,")")') le-lp-1
        read(cx(lp+2:le),cf,iostat=ios) nr
        np=le-lp-1
      endif
      nt=1
      if(lc.ge.le+2.and.cx(le+2:le+2).eq.'-') then
        le=le+1
        nt=-1
      elseif(lc.ge.le+2.and.cx(le+2:le+2).eq.'+') then
        le=le+1
        nt=+1
      endif
      ne=0
      if(lc.ge.le+2) then
        write(cf,'("(O",i2.2,")")') lc-le-1
        read(cx(le+2:lc),cf,iostat=ios) ne
      endif
      x=ns*(nl+nr*8._4**(-np))*8._4**(nt*ne)
    case('Z')
      le=index(cx(2:),'E+')
      if(le.eq.0) le=index(cx(2:),'E-')
      if(le.eq.0) le=lc
      lp=index(cx(2:le),'.')
      if(lp.eq.0) lp=le
      lb=0
      ns=1
      if(cx(2:2).eq.'+') then
        lb=1
      elseif(cx(2:2).eq.'-') then
        lb=1
        ns=-1
      endif
      nl=0
      if(lp.ge.lb+2) then
        write(cf,'("(Z",i2.2,")")') lp-lb-1
        read(cx(lb+2:lp),cf,iostat=ios) nl
      endif
      nr=0
      np=0
      if(le.ge.lp+2) then
        write(cf,'("(Z",i2.2,")")') le-lp-1
        read(cx(lp+2:le),cf,iostat=ios) nr
        np=le-lp-1
      endif
      nt=1
      if(lc.ge.le+2.and.cx(le+2:le+2).eq.'-') then
        le=le+1
        nt=-1
      elseif(lc.ge.le+2.and.cx(le+2:le+2).eq.'+') then
        le=le+1
        nt=+1
      endif
      ne=0
      if(lc.ge.le+2) then
        write(cf,'("(Z",i2.2,")")') lc-le-1
        read(cx(le+2:lc),cf,iostat=ios) ne
      endif
      x=ns*(nl+nr*16._4**(-np))*16._4**(nt*ne)
    case('z')
      np=kind(x)*2
      write(cf,'("(SS,Z",i2.2,".",i2.2,")")') np,np
      read(cx(1+1:1+np),cf,iostat=ios) x
    case default
      read(cx(1:lc),*,iostat=ios) x
    end select
    if(ios.ne.0) then
      call errmsg('fpop: error reading number '//trim(cx)) 
      call errexit(2)
    endif
  end function
  function fpop_write_4(x,cform) result(cx)
    implicit none
    real(4),intent(in):: x
    character(16),intent(in):: cform
    character(80) cx
    character(16) cf
    integer ne,np,nd,nx
    integer(4) nr
    select case(cform)
    case('I')
      if(abs(x).lt.1.) then
        if(x.lt.0.) then
          cx='-0'
        elseif(x.gt.0.) then
          cx='+0'
        else
          cx='0'
        endif
      else
        ne=floor(log10(abs(x)))+1
        write(cf,'("(SP,I",i2.2,".",i2.2,")")') ne+1,ne
        write(cx,cf) int(x,4)
      endif
    case('D')
      if(x.eq.0) then
        cx="D0"
      else
        if(x.lt.0) then
          cx="D-0."
        else
          cx="D+0."
        endif
        ne=floor(log10(abs(x)))+1
        np=precision(x)
        nr=nint(abs(x)*10._4**(-ne/2)*10._4**(-(ne+sign(1,ne))/2+np),4)
        nd=floor(log10(real(range(x))))+1
        write(cf,'("(SS,I",i2.2,".",i2.2,")")') np,np
        write(cx(4+1:4+np),cf) nr
        if(ne.lt.0) then
          cx(4+np+1:4+np+2)="E-"
        else
          cx(4+np+1:4+np+2)="E+"
        endif
        write(cf,'("(SS,I",i2.2,".",i2.2,")")') nd,nd
        write(cx(4+np+2+1:4+np+2+nd),cf) abs(ne)
      endif
    case('B')
      if(x.eq.0) then
        cx="B0"
      else
        if(x.lt.0) then
          cx="B-0."
        else
          cx="B+0."
        endif
        nx=nint(log(real(radix(x)))/log(2.))
        ne=exponent(abs(x))*nx
        np=digits(x)*nx
        nr=nint(abs(x)*2._4**(-ne/2)*2._4**(-(ne+sign(1,ne))/2+np),4)
        nd=floor(log(max(abs(minexponent(x)),maxexponent(x))*nx/1.)/log(2.))+1
        write(cf,'("(SS,B",i2.2,".",i2.2,")")') np,np
        write(cx(4+1:4+np),cf) nr
        if(ne.lt.0) then
          cx(4+np+1:4+np+2)="E-"
        else
          cx(4+np+1:4+np+2)="E+"
        endif
        write(cf,'("(SS,B",i2.2,".",i2.2,")")') nd,nd
        write(cx(4+np+2+1:4+np+2+nd),cf) abs(ne)
      endif
    case('O')
      if(x.eq.0) then
        cx="O0"
      else
        if(x.lt.0) then
          cx="O-0."
        else
          cx="O+0."
        endif
        nx=nint(log(real(radix(x)))/log(2.))
        ne=ceiling((exponent(abs(x))*nx-0.5)/3.)
        np=ceiling((digits(x)*nx-0.5)/3.)
        nr=nint(abs(x)*8._4**(-ne/2)*8._4**(-(ne+sign(1,ne))/2+np),4)
        nd=floor(log(max(abs(minexponent(x)),maxexponent(x))*nx/3.)/log(8.))+1
        write(cf,'("(SS,O",i2.2,".",i2.2,")")') np,np
        write(cx(4+1:4+np),cf) nr
        if(ne.lt.0) then
          cx(4+np+1:4+np+2)="E-"
        else
          cx(4+np+1:4+np+2)="E+"
        endif
        write(cf,'("(SS,O",i2.2,".",i2.2,")")') nd,nd
        write(cx(4+np+2+1:4+np+2+nd),cf) abs(ne)
      endif
    case('Z')
      if(x.eq.0) then
        cx="Z0"
      else
        if(x.lt.0) then
          cx="Z-0."
        else
          cx="Z+0."
        endif
        nx=nint(log(real(radix(x)))/log(2.))
        ne=ceiling((exponent(abs(x))*nx-0.5)/4.)
        np=ceiling((digits(x)*nx-0.5)/4.)
        nr=nint(abs(x)*16._4**(-ne/2)*16._4**(-(ne+sign(1,ne))/2+np),4)
        nd=floor(log(max(abs(minexponent(x)),maxexponent(x))*nx/4.)/log(16.))+1
        write(cf,'("(SS,Z",i2.2,".",i2.2,")")') np,np
        write(cx(4+1:4+np),cf) nr
        if(ne.lt.0) then
          cx(4+np+1:4+np+2)="E-"
        else
          cx(4+np+1:4+np+2)="E+"
        endif
        write(cf,'("(SS,Z",i2.2,".",i2.2,")")') nd,nd
        write(cx(4+np+2+1:4+np+2+nd),cf) abs(ne)
      endif
    case('z')
      cx="z"
      np=kind(x)*2
      write(cf,'("(SS,Z",i2.2,".",i2.2,")")') np,np
      write(cx(1+1:1+np),cf) x
    case('(  ':'(zz')
      write(cx,cform) x
    case default
      write(cx,*) x
    end select
  end function
  function fpop_query_8(cop) result(a)
    implicit none
    character*80,intent(in):: cop
    real(8):: a
    select case(cop)
    case('kind')
      a=kind(a)
    case('radix')
      a=radix(a)
    case('digits')
      a=digits(a)
    case('minexponent')
      a=minexponent(a)
    case('maxexponent')
      a=maxexponent(a)
    case('precision')
      a=precision(a)
    case('range')
      a=range(a)
    case('epsilon')
      a=epsilon(a)
    case('tiny')
      a=tiny(a)
    case('huge')
      a=huge(a)
    case default
      call errmsg('fpop: unrecognized query operation '//trim(cop))
      call errexit(2)
    end select
  end function
  function fpop_unary_8(cop,x) result(a)
    implicit none
    character*80,intent(in):: cop
    real(8),intent(in):: x
    real(8):: a
    select case(cop)
    case('-','minus')
      a=-x
    case('abs')
      a=abs(x)
    case('int')
      a=int(x,8)
    case('nint')
      a=nint(x,8)
    case('floor')
      a=floor(x,8)
    case('ceiling')
      a=ceiling(x,8)
    case('exponent')
      a=exponent(x)
    case('fraction')
      a=fraction(x)
    case('rrspacing')
      a=rrspacing(x)
    case('spacing')
      a=spacing(x)
    case('nearest+')
      a=nearest(x,+1._8)
    case('nearest-')
      a=nearest(x,-1._8)
    case('sqrt')
      a=sqrt(x)
    case('exp')
      a=exp(x)
    case('log')
      a=log(x)
    case('log10')
      a=log10(x)
    case('sinh')
      a=sinh(x)
    case('cosh')
      a=cosh(x)
    case('tanh')
      a=tanh(x)
    case('sin')
      a=sin(x)
    case('cos')
      a=cos(x)
    case('tan')
      a=tan(x)
    case('asin')
      a=asin(x)
    case('acos')
      a=acos(x)
    case('atan')
      a=atan(x)
    case('degree')
      a=x*180/acos(-1._8)
    case('radian')
      a=x*acos(-1._8)/180
    case('erf')
      a=erf(x)
    case('gamma')
      a=gamma(x)
    case default
      call errmsg('fpop: unrecognized unary operation '//trim(cop))
      call errexit(2)
    end select
  end function
  function fpop_binary_8(x,cop,y) result(a)
    implicit none
    character*80,intent(in):: cop
    real(8),intent(in):: x,y
    real(8):: a
    select case(cop)
    case('+','plus')
      a=x+y
    case('-','minus')
      a=x-y
    case('*','x','times')
      a=x*y
    case('/','over')
      a=x/y
    case('**','^','power')
      a=x**y
    case('min')
      a=min(x,y)
    case('max')
      a=max(x,y)
    case('mod')
      a=mod(x,y)
    case('modulo')
      a=modulo(x,y)
    case('atan2')
      a=atan2(x,y)
    case('round')
      a=nint(x/y,8)*y
    case('round+')
      a=ceiling(x/y,8)*y
    case('round-')
      a=floor(x/y,8)*y
    case default
      call errmsg('fpop: unrecognized binary operation '//trim(cop))
      call errexit(2)
    end select
  end function
  function fpop_read_8(cx) result(x)
    implicit none
    character*(*) cx
    real(8) x
    integer(8) nl,nr,ne
    integer lc,lp,le,lb,ns,np,nt,ios
    character(16) cf
    lc=len_trim(cx)
    select case(cx(1:1))
    case('D')
      read(cx(2:lc),*,iostat=ios) x
    case('B')
      le=index(cx(2:),'E')
      if(le.eq.0) le=lc
      lp=index(cx(2:le),'.')
      if(lp.eq.0) lp=le
      lb=0
      ns=1
      if(cx(2:2).eq.'+') then
        lb=1
      elseif(cx(2:2).eq.'-') then
        lb=1
        ns=-1
      endif
      nl=0
      if(lp.ge.lb+2) then
        write(cf,'("(B",i2.2,")")') lp-lb-1
        read(cx(lb+2:lp),cf,iostat=ios) nl
      endif
      nr=0
      np=0
      if(le.ge.lp+2) then
        write(cf,'("(B",i2.2,")")') le-lp-1
        read(cx(lp+2:le),cf,iostat=ios) nr
        np=le-lp-1
      endif
      nt=1
      if(lc.ge.le+2.and.cx(le+2:le+2).eq.'-') then
        le=le+1
        nt=-1
      elseif(lc.ge.le+2.and.cx(le+2:le+2).eq.'+') then
        le=le+1
        nt=+1
      endif
      ne=0
      if(lc.ge.le+2) then
        write(cf,'("(B",i2.2,")")') lc-le-1
        read(cx(le+2:lc),cf,iostat=ios) ne
      endif
      x=ns*(nl+nr*2._8**(-np))*2._8**(nt*ne)
    case('O')
      le=index(cx(2:),'E')
      if(le.eq.0) le=lc
      lp=index(cx(2:le),'.')
      if(lp.eq.0) lp=le
      lb=0
      ns=1
      if(cx(2:2).eq.'+') then
        lb=1
      elseif(cx(2:2).eq.'-') then
        lb=1
        ns=-1
      endif
      nl=0
      if(lp.ge.lb+2) then
        write(cf,'("(O",i2.2,")")') lp-lb-1
        read(cx(lb+2:lp),cf,iostat=ios) nl
      endif
      nr=0
      np=0
      if(le.ge.lp+2) then
        write(cf,'("(O",i2.2,")")') le-lp-1
        read(cx(lp+2:le),cf,iostat=ios) nr
        np=le-lp-1
      endif
      nt=1
      if(lc.ge.le+2.and.cx(le+2:le+2).eq.'-') then
        le=le+1
        nt=-1
      elseif(lc.ge.le+2.and.cx(le+2:le+2).eq.'+') then
        le=le+1
        nt=+1
      endif
      ne=0
      if(lc.ge.le+2) then
        write(cf,'("(O",i2.2,")")') lc-le-1
        read(cx(le+2:lc),cf,iostat=ios) ne
      endif
      x=ns*(nl+nr*8._8**(-np))*8._8**(nt*ne)
    case('Z')
      le=index(cx(2:),'E+')
      if(le.eq.0) le=index(cx(2:),'E-')
      if(le.eq.0) le=lc
      lp=index(cx(2:le),'.')
      if(lp.eq.0) lp=le
      lb=0
      ns=1
      if(cx(2:2).eq.'+') then
        lb=1
      elseif(cx(2:2).eq.'-') then
        lb=1
        ns=-1
      endif
      nl=0
      if(lp.ge.lb+2) then
        write(cf,'("(Z",i2.2,")")') lp-lb-1
        read(cx(lb+2:lp),cf,iostat=ios) nl
      endif
      nr=0
      np=0
      if(le.ge.lp+2) then
        write(cf,'("(Z",i2.2,")")') le-lp-1
        read(cx(lp+2:le),cf,iostat=ios) nr
        np=le-lp-1
      endif
      nt=1
      if(lc.ge.le+2.and.cx(le+2:le+2).eq.'-') then
        le=le+1
        nt=-1
      elseif(lc.ge.le+2.and.cx(le+2:le+2).eq.'+') then
        le=le+1
        nt=+1
      endif
      ne=0
      if(lc.ge.le+2) then
        write(cf,'("(Z",i2.2,")")') lc-le-1
        read(cx(le+2:lc),cf,iostat=ios) ne
      endif
      x=ns*(nl+nr*16._8**(-np))*16._8**(nt*ne)
    case('z')
      np=kind(x)*2
      write(cf,'("(SS,Z",i2.2,".",i2.2,")")') np,np
      read(cx(1+1:1+np),cf,iostat=ios) x
    case default
      read(cx(1:lc),*,iostat=ios) x
    end select
    if(ios.ne.0) then
      call errmsg('fpop: error reading number '//trim(cx)) 
      call errexit(2)
    endif
  end function
  function fpop_write_8(x,cform) result(cx)
    implicit none
    real(8),intent(in):: x
    character(16),intent(in):: cform
    character(80) cx
    character(16) cf
    integer ne,np,nd,nx
    integer(8) nr
    select case(cform)
    case('I')
      if(abs(x).lt.1.) then
        if(x.lt.0.) then
          cx='-0'
        elseif(x.gt.0.) then
          cx='+0'
        else
          cx='0'
        endif
      else
        ne=floor(log10(abs(x)))+1
        write(cf,'("(SP,I",i2.2,".",i2.2,")")') ne+1,ne
        write(cx,cf) int(x,8)
      endif
    case('D')
      if(x.eq.0) then
        cx="D0"
      else
        if(x.lt.0) then
          cx="D-0."
        else
          cx="D+0."
        endif
        ne=floor(log10(abs(x)))+1
        np=precision(x)
        nr=nint(abs(x)*10._8**(-ne/2)*10._8**(-(ne+sign(1,ne))/2+np),8)
        nd=floor(log10(real(range(x))))+1
        write(cf,'("(SS,I",i2.2,".",i2.2,")")') np,np
        write(cx(4+1:4+np),cf) nr
        if(ne.lt.0) then
          cx(4+np+1:4+np+2)="E-"
        else
          cx(4+np+1:4+np+2)="E+"
        endif
        write(cf,'("(SS,I",i2.2,".",i2.2,")")') nd,nd
        write(cx(4+np+2+1:4+np+2+nd),cf) abs(ne)
      endif
    case('B')
      if(x.eq.0) then
        cx="B0"
      else
        if(x.lt.0) then
          cx="B-0."
        else
          cx="B+0."
        endif
        nx=nint(log(real(radix(x)))/log(2.))
        ne=exponent(abs(x))*nx
        np=digits(x)*nx
        nr=nint(abs(x)*2._8**(-ne/2)*2._8**(-(ne+sign(1,ne))/2+np),8)
        nd=floor(log(max(abs(minexponent(x)),maxexponent(x))*nx/1.)/log(2.))+1
        write(cf,'("(SS,B",i2.2,".",i2.2,")")') np,np
        write(cx(4+1:4+np),cf) nr
        if(ne.lt.0) then
          cx(4+np+1:4+np+2)="E-"
        else
          cx(4+np+1:4+np+2)="E+"
        endif
        write(cf,'("(SS,B",i2.2,".",i2.2,")")') nd,nd
        write(cx(4+np+2+1:4+np+2+nd),cf) abs(ne)
      endif
    case('O')
      if(x.eq.0) then
        cx="O0"
      else
        if(x.lt.0) then
          cx="O-0."
        else
          cx="O+0."
        endif
        nx=nint(log(real(radix(x)))/log(2.))
        ne=ceiling((exponent(abs(x))*nx-0.5)/3.)
        np=ceiling((digits(x)*nx-0.5)/3.)
        nr=nint(abs(x)*8._8**(-ne/2)*8._8**(-(ne+sign(1,ne))/2+np),8)
        nd=floor(log(max(abs(minexponent(x)),maxexponent(x))*nx/3.)/log(8.))+1
        write(cf,'("(SS,O",i2.2,".",i2.2,")")') np,np
        write(cx(4+1:4+np),cf) nr
        if(ne.lt.0) then
          cx(4+np+1:4+np+2)="E-"
        else
          cx(4+np+1:4+np+2)="E+"
        endif
        write(cf,'("(SS,O",i2.2,".",i2.2,")")') nd,nd
        write(cx(4+np+2+1:4+np+2+nd),cf) abs(ne)
      endif
    case('Z')
      if(x.eq.0) then
        cx="Z0"
      else
        if(x.lt.0) then
          cx="Z-0."
        else
          cx="Z+0."
        endif
        nx=nint(log(real(radix(x)))/log(2.))
        ne=ceiling((exponent(abs(x))*nx-0.5)/4.)
        np=ceiling((digits(x)*nx-0.5)/4.)
        nr=nint(abs(x)*16._8**(-ne/2)*16._8**(-(ne+sign(1,ne))/2+np),8)
        nd=floor(log(max(abs(minexponent(x)),maxexponent(x))*nx/4.)/log(16.))+1
        write(cf,'("(SS,Z",i2.2,".",i2.2,")")') np,np
        write(cx(4+1:4+np),cf) nr
        if(ne.lt.0) then
          cx(4+np+1:4+np+2)="E-"
        else
          cx(4+np+1:4+np+2)="E+"
        endif
        write(cf,'("(SS,Z",i2.2,".",i2.2,")")') nd,nd
        write(cx(4+np+2+1:4+np+2+nd),cf) abs(ne)
      endif
    case('z')
      cx="z"
      np=kind(x)*2
      write(cf,'("(SS,Z",i2.2,".",i2.2,")")') np,np
      write(cx(1+1:1+np),cf) x
    case('(  ':'(zz')
      write(cx,cform) x
    case default
      write(cx,*) x
    end select
  end function
  function fpop_query_16(cop) result(a)
    implicit none
    character*80,intent(in):: cop
    real(16):: a
    select case(cop)
    case('kind')
      a=kind(a)
    case('radix')
      a=radix(a)
    case('digits')
      a=digits(a)
    case('minexponent')
      a=minexponent(a)
    case('maxexponent')
      a=maxexponent(a)
    case('precision')
      a=precision(a)
    case('range')
      a=range(a)
    case('epsilon')
      a=epsilon(a)
    case('tiny')
      a=tiny(a)
    case('huge')
      a=huge(a)
    case default
      call errmsg('fpop: unrecognized query operation '//trim(cop))
      call errexit(2)
    end select
  end function
  function fpop_unary_16(cop,x) result(a)
    implicit none
    character*80,intent(in):: cop
    real(16),intent(in):: x
    real(16):: a
    select case(cop)
    case('-','minus')
      a=-x
    case('abs')
      a=abs(x)
    case('int')
      call errmsg('fpop: kind-unsupported operation '//trim(cop))
      call errexit(2)
    case('nint')
      call errmsg('fpop: kind-unsupported operation '//trim(cop))
      call errexit(2)
    case('floor')
      call errmsg('fpop: kind-unsupported operation '//trim(cop))
      call errexit(2)
    case('ceiling')
      call errmsg('fpop: kind-unsupported operation '//trim(cop))
      call errexit(2)
    case('exponent')
      a=exponent(x)
    case('fraction')
      a=fraction(x)
    case('rrspacing')
      a=rrspacing(x)
    case('spacing')
      a=spacing(x)
    case('nearest+')
      a=nearest(x,+1._16)
    case('nearest-')
      a=nearest(x,-1._16)
    case('sqrt')
      a=sqrt(x)
    case('exp')
      a=exp(x)
    case('log')
      a=log(x)
    case('log10')
      a=log10(x)
    case('sinh')
      a=sinh(x)
    case('cosh')
      a=cosh(x)
    case('tanh')
      a=tanh(x)
    case('sin')
      a=sin(x)
    case('cos')
      a=cos(x)
    case('tan')
      a=tan(x)
    case('asin')
      a=asin(x)
    case('acos')
      a=acos(x)
    case('atan')
      a=atan(x)
    case('degree')
      a=x*180/acos(-1._16)
    case('radian')
      a=x*acos(-1._16)/180
    case('erf')
      a=erf(x)
    case('gamma')
      a=gamma(x)
    case default
      call errmsg('fpop: unrecognized unary operation '//trim(cop))
      call errexit(2)
    end select
  end function
  function fpop_binary_16(x,cop,y) result(a)
    implicit none
    character*80,intent(in):: cop
    real(16),intent(in):: x,y
    real(16):: a
    select case(cop)
    case('+','plus')
      a=x+y
    case('-','minus')
      a=x-y
    case('*','x','times')
      a=x*y
    case('/','over')
      a=x/y
    case('**','^','power')
      a=x**y
    case('min')
      a=min(x,y)
    case('max')
      a=max(x,y)
    case('mod')
      a=mod(x,y)
    case('modulo')
      a=modulo(x,y)
    case('atan2')
      a=atan2(x,y)
    case('round')
      call errmsg('fpop: kind-unsupported operation '//trim(cop))
      call errexit(2)
    case('round+')
      call errmsg('fpop: kind-unsupported operation '//trim(cop))
      call errexit(2)
    case('round-')
      call errmsg('fpop: kind-unsupported operation '//trim(cop))
      call errexit(2)
    case default
      call errmsg('fpop: unrecognized binary operation '//trim(cop))
      call errexit(2)
    end select
  end function
  function fpop_read_16(cx) result(x)
    implicit none
    character*(*) cx
    real(16) x
    integer lc,lp,le,lb,ns,np,nt,ios
    character(16) cf
    lc=len_trim(cx)
    select case(cx(1:1))
    case('D')
      read(cx(2:lc),*,iostat=ios) x
    case('B')
      call errmsg('fpop: kind-unsupported read '//cx(1:1))
      call errexit(2)
    case('O')
      call errmsg('fpop: kind-unsupported read '//cx(1:1))
      call errexit(2)
    case('Z')
      call errmsg('fpop: kind-unsupported read '//cx(1:1))
      call errexit(2)
    case('z')
      np=kind(x)*2
      write(cf,'("(SS,Z",i2.2,".",i2.2,")")') np,np
      read(cx(1+1:1+np),cf,iostat=ios) x
    case default
      read(cx(1:lc),*,iostat=ios) x
    end select
    if(ios.ne.0) then
      call errmsg('fpop: error reading number '//trim(cx)) 
      call errexit(2)
    endif
  end function
  function fpop_write_16(x,cform) result(cx)
    implicit none
    real(16),intent(in):: x
    character(16),intent(in):: cform
    character(80) cx
    character(16) cf
    integer ne,np,nd,nx
    select case(cform)
    case('I')
      call errmsg('fpop: kind-unsupported write '//cform)
      call errexit(2)
    case('D')
      call errmsg('fpop: kind-unsupported write '//cform)
      call errexit(2)
    case('B')
      call errmsg('fpop: kind-unsupported write '//cform)
      call errexit(2)
    case('O')
      call errmsg('fpop: kind-unsupported write '//cform)
      call errexit(2)
    case('Z')
      call errmsg('fpop: kind-unsupported write '//cform)
      call errexit(2)
    case('z')
      cx="z"
      np=kind(x)*2
      write(cf,'("(SS,Z",i2.2,".",i2.2,")")') np,np
      write(cx(1+1:1+np),cf) x
    case('(  ':'(zz')
      write(cx,cform) x
    case default
      write(cx,*) x
    end select
  end function
end program
