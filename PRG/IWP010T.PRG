*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : iwp010t.prg -> Transferencia de Empresa a IVA DOS
*-H
*-H Fecha de inicio :
*-H Fecha de fin    :
*-H
*-H Actualizaciones
*-H
*-H  Fecha       Programmer      Comentarios
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_pref,w_call
private all like w_*

if parameters()=0
	w_call=.f.
	w_pref=''
endif

if w0mop$'D'
	=advsuave('Versi๓n Demostraci๓n. Inhibido el acceso.')
	read when .f.
	return
endif

w_prog=upper(prompt())

*** Salvado de estados anteriores

push key clear      && Salva estados de teclas
push menu _msysmenu && Salva estado de menu

w_curobj = _curobj     && Salva numero de objeto en el read activo
w_alan   = alias()       && Salva alias vigente al llamado del programa
w_wind   = wontop()      && Salva ventana vigente al llamado del programa
w_rdlevel= rdlevel()  && Salva Nro. de read activo

*
* Verifica que no Exista nadie en el Sistema
* ------------------------------------------
d0mene=.f.

if !lockemp('')
	=errornet(075)
	d0mene=.t.
	pop key
	pop menu _msysmenu
	return
endif

sele f77
use
fieldvec[1]='F77'
use &d0f77 in 0 alias f77 excl
if neterr()
	=errornet(076)
	=login('S')
	=login('E',.t.)
	=unlockemp('')
	pop key
	pop menu _msysmenu
	return
endif

=advsuave('Se transfiere la empresa activa: '+d0coem+chr(13)+;
			 'Luego deberแ ejecutar desde el sistema IVA DOS'+chr(13)+;
			 'la opci๓n "Incorpora de IVA WINDOWS" del m๓dulo de Empresas'+chr(13)+;
			 'teniendo la misma empresa '+d0coem+' activa.')

if empty(alltrim(d0disc))
	wait window 'No hay dispositivo de IVA DOS'
else
	if !mfile(d0disc+'\iv\ivf010.dbf')
		wait window 'No se encuentra IVA DOS en '+d0disc+'\IV'
	else
		if mfile(d0disc+'\iv\transfer\ivf007.dbf') and mfile(d0disc+'\iv\transfer\ivf021.dbf')
			fieldvec[1]='f07'
			fieldvec[2]='f21'
			n0f07=d0disc+'\iv\transfer\ivf007.dbf'
			n0f21=d0disc+'\iv\transfer\ivf021.dbf'
			use &n0f07 in 0 alias f07 exclu
			use &n0f21 in 0 alias f21 exclu
			if neterr()
				w_sigue=.f.
			else
				if sino('Borra transferencia anterior','N','Transferencia a IVA DOS')='S'
					=closall()
					w_cant=adir(w_files,d0disc+'\iv\transfer\ivf???.???')
					for i=1 to w_cant
						w_file=d0disc+'\iv\transfer\'+w_files(i,1)
						delete file &w_file
					endfor
					if rmdir(d0disc+'\iv\transfer')=0
						w_sigue=.t.
					else
						w_sigue=.f.
					endif
				else
					w_sigue=.f.
				endif
			endif
		else
			w_sigue=.t.
		endif
		if w_sigue
			if sino('Transfiere la Empresa Actual','N','Transferencia a IVA DOS')='S'
				wait window 'Transfiriendo.....' nowait
				=mkdir(d0disc+'\iv\transfer')

				wait window 'Transfiriendo...f00' nowait
				sele 0
				copy file \iw\fi\00\ivf000.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f00..dbf
				use
				n0f00=d0disc+'\iv\transfer\ivf000.dbf'
				copy file &d0f99.x.dbf to &n0f00
				dele file &d0f99.x.dbf
				
				wait window 'Transfiriendo...f01' nowait
				sele 0
				copy file \iw\fi\00\ivf001.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f01..dbf
				use
				n0f01=d0disc+'\iv\transfer\ivf001.dbf'
				copy file &d0f99.x.dbf to &n0f01
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f02' nowait
				sele 0
				copy file \iw\fi\00\ivf002.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f02..dbf
				use
				n0f02=d0disc+'\iv\transfer\ivf002.dbf'
				copy file &d0f99.x.dbf to &n0f02
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f03' nowait
				sele 0
				copy file \iw\fi\00\ivf003.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f03..dbf
				use
				n0f03=d0disc+'\iv\transfer\ivf003.dbf'
				copy file &d0f99.x.dbf to &n0f03
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f04' nowait
				sele 0
				copy file \iw\fi\00\ivf004.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f04..dbf
				use
				n0f04=d0disc+'\iv\transfer\ivf004.dbf'
				copy file &d0f99.x.dbf to &n0f04
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f05' nowait
				sele 0
				copy file \iw\fi\00\ivf005.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f05..dbf
				use
				n0f05=d0disc+'\iv\transfer\ivf005.dbf'
				copy file &d0f99.x.dbf to &n0f05
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f06' nowait
				sele 0
				copy file \iw\fi\00\ivf006.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f06..dbf
				use
				n0f06=d0disc+'\iv\transfer\ivf006.dbf'
				copy file &d0f99.x.dbf to &n0f06
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f07' nowait
				sele 0
				copy file \iw\etc\ivfaux7.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				sele 0
				use &d0f07 alias f07
				do while ! eof('f07')
					scatter memvar
					set memowidth to 71
					=set_alia('f99x')
					if memlines(f07.d7memo)=0
						m.d7line=''
						=net_appe('f99x')
						gather memvar
					else
						for i=1 to memlines(f07.d7memo)
							m.d7line=mline(f07.d7memo,i)
							=net_appe('f99x')
							gather memvar
						endfor
					endif
					=rest_alia()
					set memowidth to 50
					skip in f07
				enddo
				use
				sele f99x
				use
				n0f07=d0disc+'\iv\transfer\ivf007.dbf'
				copy file &d0f99.x.dbf to &n0f07
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f13' nowait
				sele 0
				copy file \iw\fi\00\ivf013.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f13..dbf
				use
				n0f13=d0disc+'\iv\transfer\ivf013.dbf'
				copy file &d0f99.x.dbf to &n0f13
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f14' nowait
				sele 0
				copy file \iw\fi\00\ivf014.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f14..dbf
				use
				n0f14=d0disc+'\iv\transfer\ivf014.dbf'
				copy file &d0f99.x.dbf to &n0f14
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f15' nowait
				sele 0
				copy file \iw\fi\00\ivf015.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f15..dbf
				use
				n0f15=d0disc+'\iv\transfer\ivf015.dbf'
				copy file &d0f99.x.dbf to &n0f15
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f20' nowait
				sele 0
				copy file \iw\fi\00\ivf020.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f20..dbf
				use
				n0f20=d0disc+'\iv\transfer\ivf020.dbf'
				copy file &d0f99.x.dbf to &n0f20
				dele file &d0f99.x.dbf

				wait window 'Transfiriendo...f21' nowait
				sele 0
				copy file \iw\fi\ivf021.dbf to &d0f99.x.dbf
				use &d0f99.x alias f99x excl
				append from &d0f21..dbf
				use
				n0f21=d0disc+'\iv\transfer\ivf021.dbf'
				copy file &d0f99.x.dbf to &n0f21
				dele file &d0f99.x.dbf

				wait clear
			endif
		endif
	endif
endif

pop key
pop menu _msysmenu
=closall()
=unlockemp('')
sele f77
use
=login('S')
=login('E',.t.)

return

