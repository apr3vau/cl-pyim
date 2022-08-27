(defpackage cl-pyim
  (:use :clim :clim-lisp :bknr.datastore)
  (:export run-test-frame))

(in-package :cl-pyim)

;;; Pinyin utility.

(defvar *valid-shengmu*
  '(#\b #\p #\m #\f #\d #\t #\n #\l #\g #\k #\h
    #\j #\q #\x #\z #\c #\s #\r #\y #\w)
  "合法的声母

zh ch sh 双字母的就三个，函数里再检测")

(defvar *valid-pinyin*
  '("a" "e" "o" "ai" "an" "ao" "ba" "bi" "bo" "bu" "ca" "ce" "ci" "cu" "da" "de"
    "di" "du" "ei" "en" "er" "fa" "fo" "fu" "ga" "ge" "gu" "ha" "he" "hu" "ji"
    "ju" "ka" "ke" "ku" "la" "le" "li" "lo" "lu" "lv" "ma" "me" "mi" "mo" "mu"
    "na" "ne" "ng" "ni" "nu" "nv" "ou" "pa" "pi" "po" "pu" "qi" "qu" "re" "ri"
    "ru" "sa" "se" "si" "su" "ta" "te" "ti" "tu" "wa" "wo" "wu" "xi" "xu" "ya"
    "ye" "yi" "yo" "yu" "za" "ze" "zi" "zu" "ang" "bai" "ban" "bao" "bei" "ben"
    "bie" "bin" "cai" "can" "cao" "cen" "cha" "che" "chi" "chu" "cou" "cui" "cun"
    "cuo" "dai" "dan" "dao" "dei" "dia" "die" "diu" "dou" "dui" "dun" "duo" "fan"
    "fei" "fen" "fou" "gai" "gan" "gao" "gei" "gen" "gou" "gua" "gui" "gun" "guo"
    "hai" "han" "hao" "hei" "hen" "hou" "hua" "hui" "hun" "huo" "jia" "jie" "jin"
    "jiu" "jue" "jun" "kai" "kan" "kao" "ken" "kou" "kua" "kui" "kun" "kuo" "lai"
    "lan" "lao" "lei" "lia" "lie" "lin" "liu" "lou" "lun" "luo" "lve" "mai" "man"
    "mao" "mei" "men" "mie" "min" "miu" "mou" "nai" "nan" "nao" "nei" "nen" "nie"
    "nin" "niu" "nou" "nuo" "nve" "pai" "pan" "pao" "pei" "pen" "pie" "pin" "pou"
    "qia" "qie" "qin" "qiu" "que" "qun" "ran" "rao" "ren" "rou" "rui" "run" "ruo"
    "sai" "san" "sao" "sen" "sha" "she" "shi" "shu" "sou" "sui" "sun" "suo" "tai"
    "tan" "tao" "tie" "tou" "tui" "tun" "tuo" "wai" "wan" "wei" "wen" "xia" "xie"
    "xin" "xiu" "xue" "xun" "yan" "yao" "yin" "you" "yue" "yun" "zai" "zan" "zao"
    "zei" "zen" "zha" "zhe" "zhi" "zhu" "zou" "zui" "zun" "zuo" "bang" "beng"
    "bian" "biao" "bing" "cang" "ceng" "chai" "chan" "chao" "chen" "chou" "chui"
    "chun" "chuo" "cong" "cuan" "dang" "deng" "dian" "diao" "ding" "dong" "duan"
    "fang" "feng" "gang" "geng" "gong" "guai" "guan" "hang" "heng" "hong" "huai"
    "huan" "jian" "jiao" "jing" "juan" "kang" "keng" "kong" "kuai" "kuan" "lang"
    "leng" "lian" "liao" "ling" "long" "luan" "mang" "meng" "mian" "miao" "ming"
    "nang" "neng" "nian" "niao" "ning" "nong" "nuan" "pang" "peng" "pian" "piao"
    "ping" "qian" "qiao" "qing" "quan" "rang" "reng" "rong" "ruan" "sang" "seng"
    "shai" "shan" "shao" "shei" "shen" "shou" "shua" "shui" "shun" "shuo" "song"
    "suan" "tang" "teng" "tian" "tiao" "ting" "tong" "tuan" "wang" "weng" "xian"
    "xiao" "xing" "xuan" "yang" "ying" "yong" "yuan" "zang" "zeng" "zhai" "zhan"
    "zhao" "zhen" "zhou" "zhua" "zhui" "zhun" "zhuo" "zong" "zuan" "chang" "cheng"
    "chong" "chuai" "chuan" "guang" "huang" "jiang" "jiong" "kuang" "liang"
    "niang" "qiang" "qiong" "shang" "sheng" "shuai" "shuan" "xiang" "xiong"
    "zhang" "zheng" "zhong" "zhuai" "zhuan" "chuang" "shuang" "zhuang")
  "所有合法的中文拼音

按从短到长的顺序排列，这样下面split-pinyin-1就能获得从长到短的列表，便于检测")

;; (sort *valid-pinyin* #'(lambda (str1 str2)
;; 			 (< (length str1) (length str2))))

(defun split-pinyin (pinyin)
  "反复执行 'split-pinyin-1' ，直到"
  (do* ((pinyin-1 nil (split-pinyin-1 rest))
	(rest pinyin (caddr pinyin-1))
	(results nil (if (and (eql (length (car pinyin-1)) 0)
			      (eql (length (cadr pinyin-1)) 0))
			 (return (values (reverse results) (caddr pinyin-1)))
			 (push (cons (car pinyin-1)
				 (cadr pinyin-1))
			   results))))
       ((eql (length rest) 0) (reverse results))))

(defun split-pinyin-1 (pinyin)
  "将一段拼音中的第一个字的拼音切出来，和剩下的部分一起返回

\"beijingtiananmen\" => (\"b\" \"ei\" \"jingtiananmen\")"
  (let ((shengmu (aref pinyin 0))
	checklist)
    (if (member shengmu *valid-shengmu*)
	(if (and (member shengmu '(#\z #\c #\s))
		 (> (length pinyin) 1)
		 (eql (aref pinyin 1) #\h)) ;判断是否是zh ch sh		 
	    (progn
	      (dolist (valid-py *valid-pinyin*)	;生成剩下的合法拼音列表，相当于
		(if (and (eql (aref valid-py 0) shengmu) ;用声母初筛一遍
				 (eql (aref valid-py 1) #\h))
			    (push (subseq valid-py 2)				  
				  checklist))) ;这个列表是由长到短的	      
	      (setf shengmu (concatenate 'string (string shengmu) "h")))
	    (progn
	      (dolist (valid-py *valid-pinyin*) ;一位声母
		(if (and (eq (aref valid-py 0) shengmu))
			    (push (subseq valid-py 1)
					 checklist)))	      
	      (setf shengmu (string shengmu))))
	(progn
	  (dolist (valid-py *valid-pinyin*) ;无声母
	    (if (not (member (aref valid-py 0) *valid-shengmu*))	      
		      (push valid-py checklist)))
	  (setf shengmu "")))    
    ;; 进一步
    (let ((yunmu-and-rest (subseq pinyin (length shengmu)))
	  (yunmu "")
	  default)
      (block nil
	(mapc
	 #'(lambda (valid-py)
	     ;;因为push出来的列表是从长到短的，这里也就是从长到短匹配
	     (when (uiop:string-prefix-p valid-py yunmu-and-rest)
	       ;; 这里是这样一种情况：比如“相安无事”和“先干为敬”，开头都是 xiangan
	       ;; 那么我们优先选择 xian gan 而不是 xiang an ，给声韵母配对儿
	       ;; 但是这种配对仅作用于只有最后一位字母有出入的情况，
	       ;; 比如“雄安”，过掉 xiong an ，下一个合法拼音是 xi ongan，肯定不行
	       ;; 下面的代码就是在实现这样的逻辑
	       (if (and (> (length yunmu-and-rest) (length valid-py))
			(member (aref yunmu-and-rest (length valid-py))
				'(#\a #\e #\i #\o #\u)) ;如果下一个词以韵母开头
			(member (aref valid-py
				      (1- (length valid-py))) ;且本词以声母结尾
				*valid-shengmu*))
		   (setf default valid-py) ;那么先存上，过，继续减
		   (if (and default    ;如果有存的，而且长度差超过了一位，用存的
			    (> (- (length default) (length valid-py)) 1))
		       (progn (setf yunmu default)
			      (return))
		       (progn (setf yunmu valid-py)
			      (return))))))
	 checklist))
      (list shengmu yunmu (subseq yunmu-and-rest (length yunmu))))))

(defun get-shengmu (pinyin)
  "Single function to get shengmu from pinyin. Using other place.

单独的获取声母函数，用在别的地方。"
  (if (and (not (string= pinyin ""))
	   (member (aref pinyin 0) *valid-shengmu*))
      (if (and (member (aref pinyin 0) '(#\z #\s #\h))
	       (> (length pinyin) 1)
	       (eql (aref pinyin 1) #\h))
	  (subseq pinyin 0 2)
	  (string (aref pinyin 0)))
      ""))

;;; End of Pinyin utility.

;;; Dict and word utility.

(defclass word (store-object)
  ((chinese :accessor word-chinese
	    :initarg :chinese
	    :index-type bknr.indices:hash-index
	    :index-initargs (:test #'equal)
	    :index-reader word-with-chinese
	    :index-values all-chinese
	    :index-mapvalues map-word-chinese)
   (pinyin :accessor word-pinyin
	   :initarg :pinyin
	   :index-type bknr.indices:hash-index
	   :index-initargs (:test #'equal)
	   :index-reader word-with-pinyin
	   :index-values all-pinyins
	   :index-mapvalues map-word-pinyin)
   (freq :accessor word-freq
	 :initarg :freq))
  (:metaclass persistent-class))

(defun dict-init (dict-directory-path)
  "Initialize dict.

初始化词典"
  (ensure-directories-exist (pathname dict-directory-path))
  (make-instance 'mp-store
		 :directory dict-directory-path
		 :subsystems (list
			      (make-instance
			       'store-object-subsystem))))

(defun word-with-pinyin-and-chinese (pinyin chinese)
  (let ((same-pinyin (word-with-pinyin pinyin))
	(same-chinese (word-with-chinese chinese)))
    (dolist (obj same-pinyin)
      (if (member obj same-chinese)
	  (return obj)))))

(defun import-pyim-dict (pyim-dict-path)
  "Import PYIM dict into current using cl-pyim dict.

将 PYIM 词典导入当前正在使用的 cl-pyim 词典。"
  (loop for l in (uiop:read-file-lines (pathname pyim-dict-path))
	unless (equal (aref l 0) #\;)
	  do (let* ((lst (ppcre:split " " l))
		    (pinyin (ppcre:split "-" (car lst))))
	       (dolist (chinese (cdr lst))
		 (unless (word-with-pinyin-and-chinese
			  pinyin chinese)
		   (make-instance 'word
				:chinese chinese
				:pinyin pinyin
				:freq 0))))))

;; (dict-init "~/common-lisp/cl-pyim/default-dict/")
;; (import-pyim-dict "/Users/rosa/.emacs.d/straight/repos/pyim-basedict/pyim-basedict.pyim")
;; (import-pyim-dict "/Users/rosa/.emacs.d/straight/repos/pyim-tsinghua-dict/pyim-tsinghua-dict.pyim")
;; (mapcar #'word-chinese (all-chinese))
;; (mapcar #'word-pinyin (all-pinyins))
;; (mapcar #'word-chinese (word-with-pinyin '("a")))
;; (close-store)

;;; End of Dict and word utility.

;;; Matching.

(defun match-candidates (pinyin
			 &key no-whole-string-match no-partly-match)
  (multiple-value-bind (splitted invalid)
      (split-pinyin pinyin)
    (let ((first-match (match-candidates-1 splitted
					   :no-whole-string-match no-whole-string-match
					   :no-partly-match no-partly-match)))
      (when (> (length splitted) (length (word-pinyin (caar first-match))))
	(loop
	  with newcar = (word-chinese (caar first-match))
	  for last-car-length = (length newcar)
	  with car-length
	  for next-part = (car (match-candidates-1
				(subseq splitted last-car-length)
				:no-whole-string-match no-whole-string-match
				:no-partly-match no-partly-match))
	  do (setf newcar
		   (concatenate 'string
				newcar (word-chinese (car next-part)))
		   car-length (length newcar))
	  when (or (= car-length last-car-length)
		   (= (length splitted) car-length))
	    do (return (push (cons newcar
				   (uiop:if-let ((rest (cdr next-part)))
				     (concatenate 'string rest invalid)
				     invalid))
			     first-match))))
      first-match)))

(defun match-candidates-1 (splitted
			   &key no-whole-string-match no-partly-match)
  "Match word from dict. On working."
  (let (fully-match partly-match)
	  (map-word-pinyin
	   #'(lambda (obj)
	       (block current-pinyin
		 (let ((dict-pinyins (word-pinyin obj)))
		   ;; 下面这个 when 作为一个简单的初筛，可以把耗时减小五倍
		   (when (eql (aref (caar splitted) 0) (aref (car dict-pinyins) 0))

		     (when (>= (length splitted) (length dict-pinyins))
		       (loop
			 for dict-pinyin in dict-pinyins ;只有词典拼音先遍历完，该词才有效（不接受更长的）
			 for input-rest = splitted then (cdr input-rest)
			 for input = (car input-rest) ;同时遍历两组拼音对
			 for input-pinyin = (concatenate 'string
							 (car input) (cdr input))
			 with partly
			 with rest-pinyin = ""
			 do (unless	;如果没能通过下面的检验，不匹配，过
				(or (and (string= input-pinyin dict-pinyin))
				    (and (null no-partly-match)
					 (or (and (string= (cdr input) "") ;无韵母、同声母
						  (string= (car input) dict-pinyin))
					     (and (null (cdr input-rest)) ;最后一字，拼音不完整
						  (let ((li (length input-pinyin))
							(ld (length dict-pinyin)))
						    (and (<= li ld)
							 (string= input-pinyin
								  dict-pinyin
								  :end2 li)
							 (setf rest-pinyin
							       (subseq dict-pinyin
								       0 li))))))
					 (setf partly t))) ;标记部分匹配
			      (return))
			 finally (progn
				   (setf input-rest (cdr input-rest))
				   (if (or partly (and (null no-partly-match)
						       input-rest)) ;被标记或有剩余，是部分匹配
				       (push
					(cons obj
					      (apply #'concatenate 'string
						     rest-pinyin
						     (mapcan
						      #'(lambda (cell)
							  (list (car cell) (cdr cell)))
						      input-rest)))
					partly-match)
				       (push (cons obj nil) fully-match))
				   (return-from current-pinyin))))
		     
		     (if (null no-whole-string-match) ;比较整个拼音字符串是否相同
			 (let ((flatten-string (apply #'concatenate 'string ;把声韵表展开
						      (alexandria:flatten splitted))))
			   (if (and (null no-partly-match)
				    (string= (cdr (last splitted)) "")) ;最后一个拼音只有声母
			       (when (string= flatten-string ;两个拼音除了最后一个韵母外相同
					      (apply #'concatenate 'string
						     (butlast dict-pinyins)
						     (get-shengmu (last dict-pinyins))))
				 (push (list obj) partly-match) ;它是部分匹配
				 (return-from current-pinyin))
			       (when (string= flatten-string ;最后一个拼音有韵母
					      (apply #'concatenate 'string ;比较是否全同
						     dict-pinyins))
				 (push (list obj) fully-match) ;它是完全匹配
				 (return-from current-pinyin))))))))))
    (append fully-match partly-match)))

;; (time (mapcar #'word-chinese (match-candidates "xiangan")))
(time (mapcar #'(lambda (obj)
		  (if (eql (type-of obj) 'word)
		      (word-chinese obj)
		      obj))
	      (match-candidates "woaibeijingtiananmen")))

;;; End of matching.
