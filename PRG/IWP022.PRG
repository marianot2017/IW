*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP022.PRG -> Auditoria
*)-H
*)-H Fecha de inicio : 
*)-H Fecha de fin    : 
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

private all like w_*

push menu _msysmenu

if !lockemp(NF_CoEm)
   =ErrorNet(050)
else
   w_Mtitu=iif(d0audi='S', 'Auditorํa Activada', 'Auditorํa Desactivada')
   w_MAcDe=iif(d0audi='S', 'Desactivar', 'Activar')
   w_opcion=left(w_macde,1)

   if sino(' Confirma '+w_MAcDe+' ? ', 'NO', w_Mtitu)='S'

	  use &d0f10 in 0 alias f10 order p0coem
	  
	  if neterr()
         =unlockemp(NF_CoEm)
         wait window '   No Pudo Abrirse Archivo de Empresa'+chr(13)+  ;
		              '   Deben estar en elecci๓n de empresa'+chr(13)+  ;
         		      '   en otra terminal.'+chr(13)+  ;
                	  'Espere un Instante e Intntelo Nuevamente'

        pop menu _msysmenu
		return
	  endif


	  sele f10

      seek d0coem
      
      if w_opcion='D'
         repl p0audi with 'N'
         =audi('DES S','Men๚','',d0coem)
      else
         repl p0audi with 'S'
         =audi('ACT S','Men๚','',d0coem)
      endif

	  sele f10	
		
      d0audi=p0audi
      
      use
      
	  
   endif
   =unlockemp(NF_CoEm)
endif

pop menu _msysmenu
read when .f.
return
