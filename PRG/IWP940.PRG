*-P -----------------------------------------------------------------------------
*  IWP940   Resguardo
*  Release : 02
*-P -----------------------------------------------------------------------------
return
*-----------------------------------------------------------------------------
Procedure Empr940
*-----------------------------------------------------------------------------
* Parametro NF_StRe --> se utiliza en eleccion de empresa cuando
*                       se ha detectado una salida anormal en un
*                       proceso transaccional invocando EMPR940.
para NF_StRe
*
* Si no viene de Eleccion de Empresa se setea la variable.
*-----------------------------------------------------------------------------

if w0mop$'D'
	=advsuave('Versión Demostración. Inhibido el acceso.')
	read when .f.
	return
endif

w_para=parameters()
if w_para=0
   NF_StRe=.f.
   w_prfi='EM'
else
   w_prfi='ME'
endif


if !NF_StRe
    
    wait 'Este proceso realiza una COPIA de todos lo ARCHIVOS'+chr(13)+;
         'de esta EMPRESA para su posterior recupero en caso '+chr(13)+;
         'de daño de los actualmente en uso.' wind nowait	&& R.02 Mariano (Saque a disquette)

endif


push key clear      && Salva estados de teclas 
push menu _msysmenu && Salva estado de menu

set skip of menu _msysmenu .t.

s_sino = 2
w_sino=sino(' Confirma Resguardo ?','NO',upper(prompt()))	&& R.02 Mariano (w_sino=sino(' Confirma Resguardo ?'))
    
do while .t.
if w_sino='S'
   * Si NF_StRe esta en blanco se abre file si no ya esta abierto.
   * -------------------------------------------------------------
   if !NF_StRe
      Fieldvec[1] = 'F10'
      use &d0f10 in 0 alias f10 order p0coem
      if neterr()
         exit 
         *return
      endif

      =seek(d0coem,'f10')

      if .not. rlock('f10')
         =ErrorNet(054)
         sele f10
         use
         exit
         *return
      endif
   endif

   * si esta en blanco no viene de eleccion de empresas.
   * ---------------------------------------------------
   if iif(!NF_StRe,lockemp(NF_CoEm),NF_StRe)

      if !NF_StRe
         if Terano()
            =ErrorNet(004)
         endif
      endif

      sele f10

      && R.01b Mariano
      w_dire=alltr(right(f10.p0back,48))
      wDrF=left(f10.p0back,2)
      && R.01e Mariano

      priv wDire,wDriv
      wdire = alltrim(p0path)
      wdriv = wdr

      wpunt=at(':',wdire)
      if wpunt<>0
         wdriv = left(wdire,wpunt)
         wdire = subs(wdire,wpunt + 1,100)
      endif
*     copy to &wDire.Empresas.dbf	&& R.02 Mariano

      if NF_StRe
         copy file &d0pref.\iv\fil.dbf to &wDire.fil.dbf
      endif

	  && R.02b Mariano
	  w_back=.f.
	  do iwp900uh with 'E'
*	  if empty(wDrF+w_dire)	&& R.01 Mariano
*	      w_dire=''
*    	  wDrF=ElijeDisp('DESTINO DE RESGUARDO')
*     endif	&& R.01 Mariano

*     w_back=backup( wdriv , wDire , '*.*,E' ,'', wDrF ,w_dire,'IW',d0CoEm,w_prfi)
	  && R.02e Mariano

*     dele file &wDire.Empresas.dbf	&& R.02 Mariano

      if NF_StRe
         dele file &wDire.fil.dbf
      endif

      =unlockemp(NF_coEm)

      if w_back
         * Fuerza Grabacion    
         *         +               +
         =audi('RSE S','','',d0coem)
      endif

      if NF_StRe
         keyboard '{ALT+S}'
         WW_Esca=.t.
      endif

   else
      =ErrorNet(050)
   endif

	if used('f10') and !NF_StRe
    	sele f10
	    use
	endif
else
	if used('f10') and !NF_StRe
	    sele f10
    	use
	endif
	NF_StRe=.f.
endif
exit
enddo


wait clear
pop key 
pop menu _msysmenu
read when .f.

return NF_StRe

*-P -----------------------------------------------------------------------------
    Procedure Sist940
*-P -----------------------------------------------------------------------------
priv wscreen

if w0mop$'D'
	=advsuave('Versión Demostración. Inhibido el acceso')
	read when .f.
	return
endif


=AdvSuave('Deberá salir del sistema entrar al UTIL y ejecutar la opción RESGUARDO COMPLETO')
*do sist940.spr

return

* -----------------------------------------------------------------------------
Procedure Root940
* -----------------------------------------------------------------------------

if w0mop$'D'
	=advsuave('Versión Demostración. Inhibido el acceso')
	read when .f.
	return
endif

wait 'Este proceso realiza una COPIA de los ARCHIVOS'+chr(13)+;
     'del DIRECTORIO PRINCIPAL del Sistema "\IW" para'+chr(13)+;
     'su posterior recupero en el caso de daño de los'+chr(13)+;
     'actualmente en uso.'+chr(13)+;
     'Es conveniente elegir previamente la empresa 00.' wind nowait && R.02 Mariano (Saque a diskette y reestructure)

push key clear      && Salva estados de teclas 
push menu _msysmenu && Salva estado de menu
set skip of menu _msysmenu .t.

s_sino = 2
if sino(' Confirma Resguardo ? ','NO',upper(prompt()))=='S'		&& R.02 Mariano (if sino(' Confirma Resguardo ? ')=='S')
  d0mene=.f.

   if !lockemp('')
      =ErrorNet(075)
      d0mene=.t.

	  wait clear
	  pop key 
	  pop menu _msysmenu
	  read when .f.

      return
   endif

   sele f77
   use

   use &d0f77 in 0 alias f77 exclu
   if neterr()
      =ErrorNet(076)
      =login('S')
      =login('E',.t.)
      =unlockemp('')
      d0mene=.t.

	  wait clear
	  pop key 
	  pop menu _msysmenu
	  read when .f.

      return
   endif

   d0mene=.t.
   if terano()
      =ErrorNet(004)
   endif

   && R.02b Mariano	
*   w_dire=''
*   wDrF=ElijeDisp('DESTINO DE RESGUARDO')

*   w_reso=set('RESO')
*   set reso off
   
*   if used('s02')
*     sele s02
*     use
*   endif   

*   w_back=backup( wdr , '\IW\' , 'ivf*.*,N' , 'SSF*.*' , wDrF ,w_dire ,'IW',d0CoEm,'RA')

*   if .not. empty(w_reso)
*      set reso &w_reso
*   endif

	w_back=.f.
	do iwp900uh with 'S'
	&& R.02e Mariano

endif

=unlockemp('')

sele f77
use

=login('S')
=login('E',.t.)


wait clear
pop key 
pop menu _msysmenu
read when .f.

return

