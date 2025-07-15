;+
; :Author: hpc0813@outlook.com
;
;:Date: 2020-10-14 19:34:18
;-


PRO ImageView, Picture, Title
  PSize = SIZE(Picture, /Dimensions)
  PDims = [400*PSize[0]/PSize[1], 400]
  ma = MAX(Picture)
  mi = MIN(Picture)
  me = mean(Picture)
  s = stddev(Picture)
  v = variance(Picture)
  ImageView = Image(            $
    Title = Title,              $
    WINDOW_TITLE = '图片预览',   $
    Dimensions = PDims,         $
    Picture)
  t1 = TEXT(120, 1200, 'Max: '+STRTRIM(STRING(ma),2), COLOR='Red')
  t2 = TEXT(120, 1140, 'Min: '+STRTRIM(STRING(mi),2), COLOR='Red')
  t3 = TEXT(120, 1080, 'Mean: '+STRTRIM(STRING(me),2), COLOR='Red')
  t4 = TEXT(120, 1020, 'SD: '+STRTRIM(STRING(s),2), COLOR='Red')
  t6 = TEXT(120, 900, 'Var: '+STRTRIM(STRING(v),2), COLOR='Red')
  ; ImageView
END

FUNCTION cal_PDI, Nir, Red

  ;计算土壤线
  ;绘制二维特征空间
  
  NirRe = Nir
  NirRe_dims = SIZE(NirRe, /dimensions)

  RedRe = Red
  RedRe_dims = SIZE(RedRe, /dimensions)

  Nir_1D = []
  FOR i = 0, NirRe_dims[0]-1 DO BEGIN
    FOR j = 0, NirRe_dims[1]-1 DO Nir_1D = [Nir_1D, NirRe[i,j]]
  ENDFOR

  Red_1D = []
  FOR i = 0, RedRe_dims[0]-1 DO BEGIN
    FOR j = 0, RedRe_dims[1]-1 DO Red_1D = [Red_1D, RedRe[i,j]]
  ENDFOR

  ;获取初始土壤点
  ;排序后的Red_1D
  SortRed = Red_1D(SORT(Red_1D))
  ;Red_1D分组
  group = 400
  NBlocks = N_ELEMENTS(Red_1D)/group
  RedBlocks = []
  FOR i = 0, group-1 DO BEGIN
    front = i*NBlocks
    back = (i+1)*(NBlocks)-1
    RedBlocks=[[RedBlocks], [SortRed[front:back]]]
  ENDFOR
  ;求每组最小值
  Redmin=[]
  FOR i = 0, group-1 DO BEGIN
    Redmin=[Redmin, RedBlocks[0,i]]
  ENDFOR
  ;Red最小值对应的索引=与Nir对应的数据块的索引
  Nirmin=[]
  FOR i = 0, group-1 DO BEGIN
    RedminIndex = WHERE(Red_1D EQ Redmin[i])
    Nirmin = [Nirmin, MIN(Nir_1D(RedminIndex))]
  ENDFOR

  ;自适应区间及其相关性
  ;0-50%
  Block1x = Redmin[0:(group*0.5-1)]
  Block1y = Nirmin[0:(group*0.5-1)]
  Relation1 = correlate(Block1x,Block1y)
  Block1 = {BLOCK1X:Block1x, BLOCK1Y:Block1y, RELATION1:Relation1}
  ;0-75%
  Block2x = Redmin[0:(group*0.75-1)]
  Block2y = Nirmin[0:(group*0.75-1)]
  Relation2 = correlate(Block2x,Block2y)
  Block2 = {BLOCK2X:Block2x, BLOCK2Y:Block2y, RELATION2:Relation2}
  ;0-100%
  Block3x = Redmin[0:(group*1-1)]
  Block3y = Nirmin[0:(group*1-1)]
  Relation3 = correlate(Block3x,Block3y)
  Block3 = {BLOCK3X:Block3x, BLOCK3Y:Block3y, RELATION3:Relation3}
  ;25-75%
  Block4x = Redmin[(group*0.25-1):(group*0.75-1)]
  Block4y = Nirmin[(group*0.25-1):(group*0.75-1)]
  Relation4 = correlate(Block4x,Block4y)
  Block4 = {BLOCK4X:Block4x, BLOCK4Y:Block4y, RELATION4:Relation4}
  ;25-100%
  Block5x = Redmin[(group*0.25-1):(group*1-1)]
  Block5y = Nirmin[(group*0.25-1):(group*1-1)]
  Relation5 = correlate(Block5x,Block5y)
  Block5 = {BLOCK5X:Block5x, BLOCK5Y:Block5y, RELATION5:Relation5}

  ;迭代筛选土壤点
  Relation = [Relation1, Relation2, Relation3, Relation4, Relation5]

  IF Block1.(2) EQ MAX(Relation) THEN Block = Block1
  IF Block2.(2) EQ MAX(Relation) THEN Block = Block2
  IF Block3.(2) EQ MAX(Relation) THEN Block = Block3
  IF Block4.(2) EQ MAX(Relation) THEN Block = Block4
  IF Block5.(2) EQ MAX(Relation) THEN Block = Block5

  ;拟合土壤线
  M = regress(         $
    Block.(0),         $
    Block.(1),         $
    const = I,         $
    correlation = r,   $
    yfit = Nir_fitted, $
    SIGMA = sigma)

  ;返回最小值和最大值以便绘图
  Red_1D1 = MIN(Block.(0))          
  Nir_1D1 = M*FLOAT(Red_1D1) + I
  Red_1D2 = MAX(Block.(0))          
  Nir_1D2 = M*FLOAT(Red_1D2) + I

  ;作图
  scatter_PDI = plot(           $
    Block.(0),                  $
    Block.(1),                  $
    linestyle = '',             $
    symbol='o',                 $
    sym_filled = 1,             $
    WINDOW_TITLE = '图片预览',   $
    sym_size = 0.5)

  str_M = STRTRIM(STRING(M),2)
  line_PDI = plot(                                          $
    YTitle = 'Nir',                                         $
    XTitle = 'Red',                                         $
    Title = 'Nir-Red二维光谱线'+'  '+'('+'M = '+str_M + ')', $
    [Red_1D1, Red_1D2],                                     $
    [Nir_1D1, Nir_1D2],                                     $
    /overplot,                                              $
    /current,                                               $
    color = 'red')

  ;计算PDI
  Nir_type = SIZE(Nir, /type)
  Nir_dims = SIZE(Nir, /dimensions)
  arrM = MAKE_ARRAY( $
    Nir_dims[0],     $
    Nir_dims[1],     $
    type = Nir_type, $
    value = M)
  PDI = FLOAT((FLOAT(Red) + FLOAT(Nir)*arrM))/FLOAT(SQRT(arrM^2 + 1))
  RETURN, PDI
END


PRO OUTLIER_EVENT,ev

  ;"是否关闭"提示窗口
  TagName = TAG_NAMES( $
    ev,                $
    /STRUCTURE_NAME)
  PRINT, TagName
  CASE TagName OF
    'WIDGET_KILL_REQUEST':BEGIN
      tmp = DIALOG_MESSAGE( $
        '是否关闭？',        $
        title = '疑问',     $
        /question)
      IF tmp EQ 'Yes' THEN BEGIN
        WIDGET_CONTROL, ev.TOP, /destroy
        RETURN
      ENDIF ELSE BEGIN
        RETURN
      ENDELSE
    END
    ELSE:
  ENDCASE

  WIDGET_CONTROL, ev.TOP, get_uvalue = oState
  uname = WIDGET_INFO(ev.ID, /uname)

  CASE uName OF

    'bO':BEGIN
      filters = [ '*.jpg', '*.tif']
      bOfile = DIALOG_PICKFILE( $
        title = '获取波段',      $
        path = (*oState).BOPATH,$
        FILTER = filters,       $
        get_path = curPath)
      IF ~FILE_TEST(bOfile) THEN RETURN

      (*oState).BOPATH = curPath
      (*oState).BOFILEPATH = bOfile
      WIDGET_CONTROL, (*oState).BOPATHTEXT, set_value = bOfile
    END


    'OutReset':BEGIN
      TextID = WIDGET_INFO( $
        ev.TOP,             $
        Find_By_UName='OutTextX')
      IF TextID NE 0 THEN BEGIN
        ;获取输入框的值
        WIDGET_CONTROL,TextID, get_Value = OUTLIERX
      ENDIF

      TextID = WIDGET_INFO( $
        ev.TOP,             $
        Find_By_UName='OutTextY')
      IF TextID NE 0 THEN BEGIN
        ;获取输入框的值
        WIDGET_CONTROL,TextID, get_Value = OUTLIERY
      ENDIF

      WIDGET_CONTROL, (*oState).BOPATHTEXT, get_value = bOfile
      BODATA = READ_TIFF(bOfile, geotiff = GeoKeys)

      O_dims = SIZE(BODATA, /dimensions)

      NULL = 'NULL'
      NAN = 'NAN'

      ;NAN->Y
      IF ~STRCMP(OUTLIERY,NAN) && ~STRCMP(OUTLIERY, NULL) THEN BEGIN
        IF STRCMP(OUTLIERX,NAN) THEN BEGIN
          BODATA[WHERE(~FINITE(BODATA))] = FLOAT(OUTLIERY)
        ENDIF
      ENDIF
      ;X->NAN
      IF ~STRCMP(OUTLIERX,NAN) && ~STRCMP(OUTLIERX, NULL) THEN BEGIN
        IF STRCMP(OUTLIERY,NAN) THEN BEGIN
          X = FLOAT(OUTLIERX)
          Y = FLOAT(OUTLIERY)

          FOR line=0,O_dims[0]-1 DO BEGIN
            FOR piex=0,O_dims[1]-1 DO BEGIN
              IF BODATA[line,piex] EQ X THEN BEGIN
                BODATA[line,piex] = !VALUES.F_NAN
              ENDIF ELSE BEGIN
                BODATA[line,piex]=BODATA[line,piex]
              ENDELSE
            ENDFOR
          ENDFOR

        ENDIF
      ENDIF
      ;X->Y
      IF ~STRCMP(OUTLIERX,NAN) && ~STRCMP(OUTLIERX, NULL) THEN BEGIN
        IF ~STRCMP(OUTLIERY,NAN) && ~STRCMP(OUTLIERY, NULL) THEN BEGIN

          X = FLOAT(OUTLIERX)
          Y = FLOAT(OUTLIERY)

          FOR line=0,O_dims[0]-1 DO BEGIN
            FOR piex=0,O_dims[1]-1 DO BEGIN
              IF BODATA[line,piex] EQ X THEN BEGIN
                BODATA[line,piex] = Y
              ENDIF ELSE BEGIN
                BODATA[line,piex]=BODATA[line,piex]
              ENDELSE
            ENDFOR
          ENDFOR

        ENDIF
      ENDIF

      O_out = DIALOG_PICKFILE(/write, path = '', title = '请选择重置后图像的输出路径与名称')
      WRITE_TIFF, O_out, BODATA, /float, geotiff=Geokeys
      ImageView, BODATA, '重置（X->Y）'

    END

  ENDCASE
  
END

PRO OUTLIER,tlb

  ;创建界面，并关联事件
  OutBase = WIDGET_BASE(      $
    group_Leader = tlb,       $
    /column,                  $
    title ='重置输入',         $
    xoffset = 320,            $
    yoffset = 420,            $
    xsize =382,               $
    ysize =110,               $
    /TLB_KILL_REQUEST_EVENTS, $
    /TLB_FRAME_ATTR)
  WIDGET_CONTROL, OutBase, /realize

  ;波段选项
  bOTool = WIDGET_BASE(Outbase, /row)
  bOButton = WIDGET_BUTTON( $
    bOTool,                 $
    value ='波  段',          $
    uname = 'bO',           $
    xsize = 58,             $
    ysize = 25)
  bOPathText = WIDGET_TEXT(bOTool, value = '', xsize = 50) ;文件路径

  ;输入特定值X
  OutTextTool = WIDGET_BASE(Outbase, /row)
  OutTextX = CW_FIELD(          $
    OutTextTool,                $
    TITLE = "输入（X）：",       $
    xsize = 16,                 $
    /FRAME,                     $
    /ALL_EVENTS,                $
    uname = 'OutTextX')
  OutTextY = CW_FIELD(          $
    OutTextTool,                $
    TITLE = "输入（Y）：",       $
    xsize = 17,                 $
    /FRAME,                     $
    /ALL_EVENTS,                $
    uname = 'OutTextY')
    
  ;异常值菜单
  OutReset = WIDGET_BUTTON(  $
    Outbase,                 $
    value ='重置（X -> Y）',  $
    uname = 'OutReset',      $
    ysize = 25)

  ;传递变量
  oState = {                  $
    BOPATH:'',                $
    BOFILEPATH:'',            $
    BOPATHTEXT:bOpathText}
  WIDGET_CONTROL, Outbase, set_uvalue = PTR_NEW(oState)

  XMANAGER, 'OUTLIER', OutBase, /no_Block
END


PRO PREVIEW_EVENT,ev

  ;"是否关闭"提示窗口
  TagName = TAG_NAMES( $
    ev,                $
    /STRUCTURE_NAME)
  PRINT, TagName
  CASE TagName OF
    'WIDGET_KILL_REQUEST':BEGIN
      tmp = DIALOG_MESSAGE( $
        '是否关闭？',        $
        title = '疑问',     $
        /question)
      IF tmp EQ 'Yes' THEN BEGIN
        WIDGET_CONTROL, ev.TOP, /destroy
        RETURN
      ENDIF ELSE BEGIN
        RETURN
      ENDELSE
    END
    ELSE:
  ENDCASE

  WIDGET_CONTROL, ev.TOP, get_uvalue = prevState
  uname = WIDGET_INFO(ev.ID, /uname)

  CASE uName OF

    'bPreV':BEGIN
      filters = [ '*.jpg', '*.tif']
      bPreVfile = DIALOG_PICKFILE(    $
        title = '获取波段',            $
        path = (*prevState).BPREVPATH,$
        FILTER = filters,             $
        get_path = curPath)
      IF ~FILE_TEST(bPreVfile) THEN RETURN

      (*prevState).BPREVPATH = curPath
      (*prevState).BPREVFILEPATH = bPreVfile
      WIDGET_CONTROL, (*prevState).BPREVPATHTEXT, set_value = bPreVfile
    END

    'PreView':BEGIN   
      WIDGET_CONTROL, (*prevState).BPREVPATHTEXT, get_value = bPreVfile
      BPREVDATA = READ_TIFF(bPreVfile, geotiff = GeoKeys)
      
      ;利用Widget_info来获得OutText的ID
      TextID = WIDGET_INFO( $
        ev.TOP,             $
        Find_By_UName='PreVText')
      IF TextID NE 0 THEN BEGIN
        ;获取输入框的值
        WIDGET_CONTROL,TextID, get_Value = oValue
        PREVTITLE = oValue
      ENDIF
      
      ImageView, BPREVDATA, PREVTITLE
    END

  ENDCASE
END

PRO PreView,tlb

  ;创建界面，并关联事件
  PreVBase = WIDGET_BASE(      $
    group_Leader = tlb,        $
    /column,                   $
    title ='预览输入',          $
    xoffset = 1220,             $
    yoffset = 420,             $
    xsize =385,                $
    ysize =130,                $
    /TLB_KILL_REQUEST_EVENTS,  $
    /TLB_FRAME_ATTR)
  WIDGET_CONTROL, PreVBase, /realize

  ;波段选项
  bPreVTool = WIDGET_BASE(PreVBase, /row)
  bPreVButton = WIDGET_BUTTON( $
    bPreVTool,                 $
    value ='波  段',             $
    uname = 'bPreV',           $
    xsize = 59,                $
    ysize = 32)
  bPreVPathText = WIDGET_TEXT(bPreVTool, value = '', xsize = 50) ;文件路径

  PreVTextTool = WIDGET_BASE(PreVBase, /row)
  PreVText = CW_FIELD( $
    PreVTextTool,      $
    TITLE = '标题：',   $
    xsize = 53,        $
    ysize = 1,         $
    /FRAME,            $
    /ALL_EVENTS,       $
    uname = 'PreVText')

  PreView = WIDGET_BUTTON(  $
    PreVBase,               $
    value ='预览',           $
    uname = 'PreView',      $
    ysize = 32)

  ;传递变量
  prevState = {                  $
    BPREVPATH:'',                $
    BPREVFILEPATH:'',            $
    BPREVPATHTEXT:bPreVPathText}
  WIDGET_CONTROL, PreVBase, set_uvalue = PTR_NEW(prevState)


  XMANAGER, 'PreView', PreVBase, /no_Block
END

PRO Soil_Moisture_EVENT, EV
  HELP, ev

  ;"是否关闭"提示窗口
  TagName = TAG_NAMES( $
    ev,                $
    /STRUCTURE_NAME)
  PRINT, TagName
  CASE TagName OF
    'WIDGET_KILL_REQUEST':BEGIN
      tmp = DIALOG_MESSAGE( $
        '是否关闭？',        $
        title = '疑问',     $
        /question)
      IF tmp EQ 'Yes' THEN BEGIN
        WIDGET_CONTROL, ev.TOP, /destroy
        RETURN
      ENDIF ELSE BEGIN
        RETURN
      ENDELSE
    END
    ELSE:
  ENDCASE

  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  uname = WIDGET_INFO(ev.ID, /uname)
  PRINT, uname

  CASE uname OF
    ;菜单功能
    'Outlier':BEGIN
      Outlier, ev.TOP
    END

    'Normalization':BEGIN
      Normalization, ev.TOP
    END

    'PreView':BEGIN
      PreView, ev.TOP
    END

    ;波段数据存储
    'bR':BEGIN
      filters = [ '*.jpg', '*.tif']
      bRfile = DIALOG_PICKFILE(  $
        title = '获取红光波段',   $
        path = (*pState).BRPATH, $
        FILTER = filters,        $
        get_path = curPath)
      IF ~FILE_TEST(bRfile) THEN RETURN

      (*pState).BRPATH = curPath
      (*pState).BRFILEPATH = bRfile
      WIDGET_CONTROL, (*pState).BRPATHTEXT, set_value = bRfile
    END


    'bNir':BEGIN
      filters = [ '*.jpg', '*.tif']
      bNirfile = DIALOG_PICKFILE( $
        title = '获取近红外波段',  $
        path = (*pState).BNIRPATH,$
        FILTER = filters,         $
        get_path = curPath)
      IF ~FILE_TEST(bNirfile) THEN RETURN

      (*pState).BNIRPATH = curPath
      (*pState).BNIRFILEPATH = bNirfile
      WIDGET_CONTROL, (*pState).BNIRPATHTEXT, set_value = bNirfile

    END

    ;指数计算
    'bPDI':BEGIN

      WIDGET_CONTROL, (*pState).BNIRPATHTEXT, get_value = bNirfile
      BNIRDATA = READ_TIFF(bNirfile, geotiff = GeoKeys)

      WIDGET_CONTROL, (*pState).BRPATHTEXT, get_value = bRfile
      BRDATA = READ_TIFF(bRfile, geotiff = GeoKeys)

      PDI = cal_PDI(BNIRDATA, BRDATA)
      PDI_out = DIALOG_PICKFILE(/write, path = '', title = '请选择PDI输出的图像路径与名称')
      WRITE_TIFF, PDI_out, PDI, /float, geotiff=Geokeys
      ImageView, PDI, '垂直干旱指数(PDI)'

    END

    'bNDVI':BEGIN
      WIDGET_CONTROL, (*pState).BRPATHTEXT, get_value = bRfile
      BRDATA = READ_TIFF(bRfile, geotiff = GeoKeys)

      WIDGET_CONTROL, (*pState).BNIRPATHTEXT, get_value = bNirfile
      BNIRDATA = READ_TIFF(bNirfile, geotiff = GeoKeys)

      NDVI = FLOAT(FLOAT(BNIRDATA)-FLOAT(BRDATA))/FLOAT(FLOAT(BNIRDATA)+FLOAT(BRDATA))
      NDVI_out = DIALOG_PICKFILE(/write, path = '', title = '请选择NDVI输出的图像路径与名称')
      WRITE_TIFF, NDVI_out, NDVI, /float, geotiff=Geokeys
      ImageView, NDVI, '归一化植被指数（NDVI）'
    END

    'bVFC':BEGIN
      WIDGET_CONTROL, (*pState).BRPATHTEXT, get_value = bRfile
      BRDATA = READ_TIFF(bRfile, geotiff = GeoKeys)

      WIDGET_CONTROL, (*pState).BNIRPATHTEXT, get_value = bNirfile
      BNIRDATA = READ_TIFF(bNirfile, geotiff = GeoKeys)

      NDVI = FLOAT(FLOAT(BNIRDATA)-FLOAT(BRDATA))/FLOAT(FLOAT(BNIRDATA)+FLOAT(BRDATA))

      ;输入为NDVI,根据NDVI统计5%和95%的置信区间
      w = WHERE(NDVI GT 0,count)
      ht = HISTOGRAM(NDVI[w],nbins=1000,locations=locations)
      ht_acc = TOTAL(ht,/cumulative)/count

      ;5%的累计区间
      w1 = WHERE(ht_acc GT 0.05)
      NDVI_soil = locations[w1[0]-1]
      ;95%的累计区间
      w2 = WHERE(ht_acc GE 0.95)
      NDVI_veg = locations(w2[0])

      ImageView, NDVI, '归一化植被指数（NDVI）'
      t7 = TEXT(120, 840, '5%置信区间: '+STRTRIM(STRING(NDVI_soil),2)+'（NDVI_Soil）', COLOR='Red')
      t8 = TEXT(120, 780, '95%置信区间: '+STRTRIM(STRING(NDVI_veg),2)+'（NDVI_Veg）', COLOR='Red')

      VFC = (FLOAT(NDVI)-NDVI_soil)/(NDVI_veg-NDVI_soil)
      VFC_out = DIALOG_PICKFILE(/write, path = '', title = '请选择VFC输出的图像路径与名称')
      WRITE_TIFF, VFC_out, VFC, /float, geotiff=Geokeys

      ImageView, VFC, '植被覆盖度（VFC）'
    END

    ELSE:
  ENDCASE

END

PRO Soil_Moisture

  ;字体设置
  sysFont = !P.FONT
  !P.FONT = 0

  ;主界面
  tlb = WIDGET_BASE(title = 'Soil_Moisture',     $
    scr_xsize = 500,                      $
    scr_ysize = 300,                      $
    map = 0,                              $
    /column,                              $
    /TLB_KILL_REQUEST_EVENTS)
  WIDGET_CONTROL, tlb, /realize, map =0

  ;菜单
  Mbar = WIDGET_BASE(tlb, /row)
  Outlier = WIDGET_BUTTON( $
    Mbar,                  $
    uname = 'Outlier',     $
    value = '异常值',       $
    xsize = 230,           $
    ysize = 38)

  PreView = WIDGET_BUTTON(    $
    Mbar,                     $
    uname = 'PreView',        $
    value = '预览',            $
    xsize = 230,              $
    ysize = 38)


  ;波段选项
  bRTool = WIDGET_BASE(tlb, /row)
  bRButton = WIDGET_BUTTON(bRTool, $
    value ='R（红光波段）',         $
    uname = 'bR',                  $
    xsize = 120,                   $
    ysize = 33)
  bRPathText = WIDGET_TEXT(bRTool, value = '', xsize = 55) ;文件路径


  bNirTool = WIDGET_BASE(tlb, /row)
  bNir = WIDGET_BUTTON(bNirTool, $
    value ='Nir（近红外波段）',   $
    uname = 'bNir',              $
    xsize = 120,                 $
    ysize = 33)
  bNirPathText = WIDGET_TEXT(bNirTool, value = '', xsize = 55) ;文件路径



  path = ROUTINE_FILEPATH('Soil_Moisture')
  dir = FILE_DIRNAME(path)
  pngpath = dir + '\resources'

  ;创建指数界面
  PDITool = WIDGET_BASE(tlb, /row)
  PDI = WIDGET_BUTTON(PDITool, $
    value ='PDI',              $
    uname = 'bPDI',            $
    xsize = 120)
  dPDI = WIDGET_DRAW(PDITool, $
    xsize = [345],            $
    ysize = [50])
  pngPDI = read_image(pngpath + '\PDI.tif')
  pPDI = reverse(pngPDI,2)
  TV, pPDI;, true = 1
  WIDGET_CONTROL, PDITool, map = 1

  VFCTool = WIDGET_BASE(tlb, /row)
  VFC = WIDGET_BUTTON(VFCTool, $
    value ='VFC',              $
    uname = 'bVFC',            $
    xsize = 120)
  dVFC = WIDGET_DRAW(VFCTool, $
    xsize = [345],            $
    ysize = [50])
  pngVFC = read_image(pngpath + '\VFC.tif')
  pVFC = reverse(pngVFC,2)
  TV, pVFC;, true = 1
  WIDGET_CONTROL, VFCTool, map = 1

  WIDGET_CONTROL, tlb, map = 1

  ;传递变量
  pState = {                  $
    BRPATH:'',                $
    BRFILEPATH:'',            $
    BRPATHTEXT:bRpathText,    $
    BNIRPATH:'',              $
    BNIRFILEPATH:'',          $
    BNIRPATHTEXT:bNirpathText,$
    PATH:path,                $
    DIR:dir}
  WIDGET_CONTROL, tlb, set_uvalue = PTR_NEW(pState)

  ;事件管理
  xmanager, 'Soil_Moisture', tlb, /no_block
END
