;; -*- Gerbil -*-

(import :std/error
        :std/foreign
        :std/format
        :gerbil/gambit

        :unicorn/arm64
        :unicorn/arm
        :unicorn/x86)

(export version
        architecture-supported?
        make-engine

        UC_ARCH_ARM
        UC_ARCH_ARM64
        UC_ARCH_MIPS
        UC_ARCH_X86
        UC_ARCH_PPC
        UC_ARCH_SPARC
        UC_ARCH_M68K
        UC_ARCH_MAX

        UC_MODE_LITTLE_ENDIAN
        UC_MODE_BIG_ENDIAN
        UC_MODE_ARM
        UC_MODE_THUMB
        UC_MODE_MCLASS
        UC_MODE_V8
        UC_MODE_MICRO
        UC_MODE_MIPS3
        UC_MODE_MIPS32R6
        UC_MODE_MIPS32
        UC_MODE_MIPS64
        UC_MODE_16
        UC_MODE_32
        UC_MODE_64
        UC_MODE_PPC32
        UC_MODE_PPC64
        UC_MODE_QPX
        UC_MODE_SPARC32
        UC_MODE_SPARC64
        UC_MODE_V9

        UC_ERR_OK
        UC_ERR_NOMEM
        UC_ERR_ARCH
        UC_ERR_HANDLE
        UC_ERR_MODE
        UC_ERR_VERSION
        UC_ERR_READ_UNMAPPED
        UC_ERR_WRITE_UNMAPPED
        UC_ERR_FETCH_UNMAPPED
        UC_ERR_HOOK
        UC_ERR_INSN_INVALID
        UC_ERR_MAP
        UC_ERR_WRITE_PROT
        UC_ERR_READ_PROT
        UC_ERR_FETCH_PROT
        UC_ERR_ARG
        UC_ERR_READ_UNALIGNED
        UC_ERR_WRITE_UNALIGNED
        UC_ERR_FETCH_UNALIGNED
        UC_ERR_HOOK_EXIST
        UC_ERR_RESOURCE
        UC_ERR_EXCEPTION

        UC_HOOK_INTR
        UC_HOOK_INSN
        UC_HOOK_CODE
        UC_HOOK_BLOCK
        UC_HOOK_MEM_READ_UNMAPPED
        UC_HOOK_MEM_WRITE_UNMAPPED
        UC_HOOK_MEM_FETCH_UNMAPPED
        UC_HOOK_MEM_READ_PROT
        UC_HOOK_MEM_WRITE_PROT
        UC_HOOK_MEM_FETCH_PROT
        UC_HOOK_MEM_READ
        UC_HOOK_MEM_WRITE
        UC_HOOK_MEM_FETCH
        UC_HOOK_MEM_READ_AFTER)

(defstruct (unicorn-error <error>) (error-code error-message))

(def (raise-unicorn-error code)
  (raise (make-unicorn-error code (uc_strerror code))))

(def (version)
  (def combined (uc_version))
  (def major (##fxarithmetic-shift-right combined 8))
  (def minor (bitwise-and combined #xff))
  [major minor combined])

(def (version-string)
  (with ([major minor _] (version))
    (format "~d.~d" major minor)))

(def (architecture-supported? arch)
  (uc_arch_supported arch))

(defstruct engine (uc unicorn-version hooks)
  final: #t
  constructor: :init!)

(defmethod {:init! engine}
  (lambda (self arch: arch mode: mode)
    (def uc (uc_make_engine arch mode))
    (unless uc
      (raise-unicorn-error UC_ERR_ARG))
    (set! (engine-uc self) uc)
    (set! (engine-unicorn-version self) (version))
    (set! (engine-hooks self) (make-hash-table))))

(defmethod {write-register engine}
  (lambda (self register value)
    (with ((engine uc _ _) self)
      (let (err (uc_reg_write uc register value))
        (unless (= err UC_ERR_OK)
          (raise-unicorn-error err))))))

(defmethod {read-register engine}
  (lambda (self register)
    (with ((engine uc _ _) self)
      (uc_reg_read uc register))))

(defmethod {write-memory engine}
  (lambda (self address bytes)
    (with ((engine uc _ _) self)
      (uc_mem_write uc address bytes))))

(defmethod {read-memory engine}
  (lambda (self address size)
    (def uc (engine-uc self))
    (let (bytes (make-u8vector size))
      (let (err (uc_mem_read uc address bytes size))
        (if (= err UC_ERR_OK)
          bytes
          (raise-unicorn-error err))))))

(defmethod {start-emulation engine}
  (lambda (self begin: begin         until: until
                timeout: (timeout 0) count: (count 0))
    (let (err (uc_emu_start (engine-uc self) begin until timeout count))
      (unless (= err UC_ERR_OK)
        (raise-unicorn-error err)))))

(defmethod {stop-emultation engine}
  (lambda (self)
    (let (err (uc_emu_stop (engine-uc self)))
      (unless (= err UC_ERR_OK)
        (raise-unicorn-error err)))))

(defmethod {add-hook engine}
  (lambda (self hook-type callback
                user-data: (user-data #f)
                begin: (begin 1)
                end:   (end 0)
                arg1:  (arg1 0))
    (let (data [self callback user-data])
      (let (hook (uc_hook_add (engine-uc self) hook-type data begin end arg1))
        (unless (= hook -1)
          (hash-put! (engine-hooks self) hook hook))))))

(defmethod {remove-hook engine}
  (lambda (self hook)
    (let (err (uc_hook_del (engine-uc self) hook))
      (unless (= err UC_ERR_OK)
        (raise-unicorn-error err)))))

(defmethod {map-memory engine}
  (lambda (self address size
                all:   (all #f)   read:    (read #f)
                write: (write #f) execute: (execute #f))
    (def uc (engine-uc self))
    (def perm
      (bitwise-ior
       (if all UC_PROT_ALL 0)
       (if read UC_PROT_READ 0)
       (if write UC_PROT_WRITE 0)
       (if execute UC_PROT_EXEC 0)))
    (def err (uc_mem_map uc address size perm))
    (unless (= err UC_ERR_OK)
      (raise-unicorn-error err))))

(defmethod {unmap-memory engine}
  (lambda (self address size)
    (let (err (uc_mem_unmap (engine-uc self) address size))
      (unless (= err UC_ERR_OK)
        (raise-unicorn-error err)))))

(begin-ffi (uc_arch
            uc_mode
            uc_err
            uc_version
            uc_arch_supported
            uc_make_engine
            uc_strerror
            uc_reg_write
            uc_reg_read
            uc_mem_write
            uc_mem_read
            uc_emu_start
            uc_emu_stop
            uc_hook_add
            uc_hook_del
            uc_mem_map
            uc_mem_unmap

            UC_ARCH_ARM
            UC_ARCH_ARM64
            UC_ARCH_MIPS
            UC_ARCH_X86
            UC_ARCH_PPC
            UC_ARCH_SPARC
            UC_ARCH_M68K
            UC_ARCH_MAX

            UC_MODE_LITTLE_ENDIAN
            UC_MODE_BIG_ENDIAN
            UC_MODE_ARM
            UC_MODE_THUMB
            UC_MODE_MCLASS
            UC_MODE_V8
            UC_MODE_MICRO
            UC_MODE_MIPS3
            UC_MODE_MIPS32R6
            UC_MODE_MIPS32
            UC_MODE_MIPS64
            UC_MODE_16
            UC_MODE_32
            UC_MODE_64
            UC_MODE_PPC32
            UC_MODE_PPC64
            UC_MODE_QPX
            UC_MODE_SPARC32
            UC_MODE_SPARC64
            UC_MODE_V9

            UC_ERR_OK
            UC_ERR_NOMEM
            UC_ERR_ARCH
            UC_ERR_HANDLE
            UC_ERR_MODE
            UC_ERR_VERSION
            UC_ERR_READ_UNMAPPED
            UC_ERR_WRITE_UNMAPPED
            UC_ERR_FETCH_UNMAPPED
            UC_ERR_HOOK
            UC_ERR_INSN_INVALID
            UC_ERR_MAP
            UC_ERR_WRITE_PROT
            UC_ERR_READ_PROT
            UC_ERR_FETCH_PROT
            UC_ERR_ARG
            UC_ERR_READ_UNALIGNED
            UC_ERR_WRITE_UNALIGNED
            UC_ERR_FETCH_UNALIGNED
            UC_ERR_HOOK_EXIST
            UC_ERR_RESOURCE
            UC_ERR_EXCEPTION

            UC_MEM_READ
            UC_MEM_WRITE
            UC_MEM_FETCH
            UC_MEM_READ_UNMAPPED
            UC_MEM_WRITE_UNMAPPED
            UC_MEM_FETCH_UNMAPPED
            UC_MEM_WRITE_PROT
            UC_MEM_READ_PROT
            UC_MEM_FETCH_PROT
            UC_MEM_READ_AFTER

            UC_HOOK_INTR
            UC_HOOK_INSN
            UC_HOOK_CODE
            UC_HOOK_BLOCK
            UC_HOOK_MEM_READ_UNMAPPED
            UC_HOOK_MEM_WRITE_UNMAPPED
            UC_HOOK_MEM_FETCH_UNMAPPED
            UC_HOOK_MEM_READ_PROT
            UC_HOOK_MEM_WRITE_PROT
            UC_HOOK_MEM_FETCH_PROT
            UC_HOOK_MEM_READ
            UC_HOOK_MEM_WRITE
            UC_HOOK_MEM_FETCH
            UC_HOOK_MEM_READ_AFTER

            UC_QUERY_MODE
            UC_QUERY_PAGE_SIZE
            UC_QUERY_ARCH

            UC_PROT_NONE
            UC_PROT_READ
            UC_PROT_WRITE
            UC_PROT_EXEC
            UC_PROT_ALL)

  (define-macro (defenum name-and-c-name . enum-values)
    (let ((name (car name-and-c-name))
          (c-name (cadr name-and-c-name)))
      `(begin
         (c-define-type ,name unsigned-int)
         ,@(map (lambda (enum) `(define-const ,enum)) enum-values))))

  (c-declare #<<END-C
#include <unicorn/unicorn.h>
#include <unicorn/x86.h>

#ifndef __HAVE_FFI_U8VECTOR
#define __HAVE_FFI_U8VECTOR
#define U8_DATA(obj) ___CAST (___U8*, ___BODY_AS (obj, ___tSUBTYPED))
#define U8_LEN(obj) ___HD_BYTES (___HEADER (obj))
#endif

static void ffi_uc_close(void *);

END-C
)

  (defenum (uc_arch "uc_arch")
    UC_ARCH_ARM
    UC_ARCH_ARM64
    UC_ARCH_MIPS
    UC_ARCH_X86
    UC_ARCH_PPC
    UC_ARCH_SPARC
    UC_ARCH_M68K
    UC_ARCH_MAX)

  (defenum (uc_mode "uc_mode")
    UC_MODE_LITTLE_ENDIAN
    UC_MODE_BIG_ENDIAN
    UC_MODE_ARM
    UC_MODE_THUMB
    UC_MODE_MCLASS
    UC_MODE_V8
    UC_MODE_MICRO
    UC_MODE_MIPS3
    UC_MODE_MIPS32R6
    UC_MODE_MIPS32
    UC_MODE_MIPS64
    UC_MODE_16
    UC_MODE_32
    UC_MODE_64
    UC_MODE_PPC32
    UC_MODE_PPC64
    UC_MODE_QPX
    UC_MODE_SPARC32
    UC_MODE_SPARC64
    UC_MODE_V9)

  (defenum (uc_err "uc_err")
    UC_ERR_OK
    UC_ERR_NOMEM
    UC_ERR_ARCH
    UC_ERR_HANDLE
    UC_ERR_MODE
    UC_ERR_VERSION
    UC_ERR_READ_UNMAPPED
    UC_ERR_WRITE_UNMAPPED
    UC_ERR_FETCH_UNMAPPED
    UC_ERR_HOOK
    UC_ERR_INSN_INVALID
    UC_ERR_MAP
    UC_ERR_WRITE_PROT
    UC_ERR_READ_PROT
    UC_ERR_FETCH_PROT
    UC_ERR_ARG
    UC_ERR_READ_UNALIGNED
    UC_ERR_WRITE_UNALIGNED
    UC_ERR_FETCH_UNALIGNED
    UC_ERR_HOOK_EXIST
    UC_ERR_RESOURCE
    UC_ERR_EXCEPTION)

  (c-define-type uc_struct (struct "uc_struct"))
  (c-define-type uc_engine uc_struct)
  (c-define-type uc_engine* (pointer uc_engine uc_engine* "ffi_uc_close"))
  (c-define-type uc_hook size_t)

  (defenum (uc_mem_type "uc_mem_type")
    UC_MEM_READ
    UC_MEM_WRITE
    UC_MEM_FETCH
    UC_MEM_READ_UNMAPPED
    UC_MEM_WRITE_UNMAPPED
    UC_MEM_FETCH_UNMAPPED
    UC_MEM_WRITE_PROT
    UC_MEM_READ_PROT
    UC_MEM_FETCH_PROT
    UC_MEM_READ_AFTER)

  (defenum (uc_hook_type "uc_hook_type")
    UC_HOOK_INTR
    UC_HOOK_INSN
    UC_HOOK_CODE
    UC_HOOK_BLOCK
    UC_HOOK_MEM_READ_UNMAPPED
    UC_HOOK_MEM_WRITE_UNMAPPED
    UC_HOOK_MEM_FETCH_UNMAPPED
    UC_HOOK_MEM_READ_PROT
    UC_HOOK_MEM_WRITE_PROT
    UC_HOOK_MEM_FETCH_PROT
    UC_HOOK_MEM_READ
    UC_HOOK_MEM_WRITE
    UC_HOOK_MEM_FETCH
    UC_HOOK_MEM_READ_AFTER)

  (defenum (uc_query_type "uc_query_type")
    UC_QUERY_MODE
    UC_QUERY_PAGE_SIZE
    UC_QUERY_ARCH)

  (define-const UC_HOOK_MEM_UNMAPPED)
  (define-const UC_HOOK_MEM_PROT)
  (define-const UC_HOOK_MEM_READ_INVALID)
  (define-const UC_HOOK_MEM_WRITE_INVALID)
  (define-const UC_HOOK_MEM_FETCH_INVALID)
  (define-const UC_HOOK_MEM_INVALID)
  (define-const UC_HOOK_MEM_VALID)

  (defenum (uc_prot "uc_prot")
    UC_PROT_NONE
    UC_PROT_READ
    UC_PROT_WRITE
    UC_PROT_EXEC
    UC_PROT_ALL)

  (c-define-type uc_mem_region "uc_mem_region")
  (c-define-type uc_mem_region* (pointer uc_mem_region))
  (c-define-type uc_context "uc_context")

  (define-c-lambda uc_mem_region_begin (uc_mem_region*) unsigned-long-long
    "___return (___arg1->begin);")
  (define-c-lambda uc_mem_region_end (uc_mem_region*) unsigned-long-long
    "___return (___arg1->end);")
  (define-c-lambda uc_mem_region_end (uc_mem_region*) unsigned-long-long
    "___return (___arg1->perms);")
  (define-c-lambda uc_version () unsigned-int
    "___return (uc_version(NULL, NULL));")
  (define-c-lambda uc_arch_supported (uc_arch) bool
    "uc_arch_supported")
  (define-c-lambda uc_make_engine (uc_arch uc_mode) uc_engine*
    "ffi_uc_make_engine")
  (define-c-lambda uc_close (uc_engine*) uc_err
    "uc_close")
  ;; (define-c-lambda uc_query (uc_engine* uc_query_type) size_t
  ;;   "uc_query")
  (define-c-lambda uc_errno (uc_engine*) uc_err
    "uc_errno")
  (define-c-lambda uc_strerror (uc_err) char-string
    "uc_strerror")
  (define-c-lambda uc_reg_write (uc_engine* int unsigned-int64) uc_err
    "ffi_uc_reg_write")
  (define-c-lambda uc_reg_read (uc_engine* int) unsigned-int64
    "ffi_uc_reg_read")
  (define-c-lambda uc_mem_write (uc_engine* unsigned-int64 scheme-object) uc_err
    "ffi_uc_mem_write")
  (define-c-lambda uc_mem_read (uc_engine* unsigned-int64 scheme-object size_t) uc_err
    "ffi_uc_mem_read")
  (define-c-lambda uc_emu_start (uc_engine* unsigned-int64 unsigned-int64 unsigned-int64 size_t)
    uc_err
    "uc_emu_start")
  (define-c-lambda uc_emu_stop (uc_engine*) uc_err
    "uc_emu_stop;")
  (define-c-lambda uc_hook_add (uc_engine* int scheme-object unsigned-int64 unsigned-int64 int)
    uc_hook
    "ffi_uc_hook_add")
  (define-c-lambda uc_hook_del (uc_engine* uc_hook) uc_err
    "uc_hook_del")
  (define-c-lambda uc_mem_map (uc_engine* unsigned-int64 size_t unsigned-int32) uc_err
    "uc_mem_map")
  (define-c-lambda uc_mem_unmap (uc_engine* unsigned-int64 size_t) uc_err
    "uc_mem_unmap")
  (define-c-lambda uc_mem_protect (uc_engine* unsigned-int64 size_t unsigned-int32) uc_err
    "uc_mem_protect")

(c-define (hookcode_cb engine address size user-data)
          (uc_engine* unsigned-int64 unsigned-int32 scheme-object)
          void "hookcode_cb" "static"
          (let ((scm-engine (car user-data))
                (callback (cadr user-data))
                (user-data0 (caddr user-data)))
            (callback scm-engine address size user-data0)))

(c-define (hookintr_cb engine intno user-data)
          (uc_engine* unsigned-int32 scheme-object)
          void "hookintr_cb" "static"
          (let ((scm-engine (car user-data))
                (callback (cadr user-data))
                (user-data0 (caddr user-data)))
            (callback scm-engine intno user-data0)))

(c-define (hookinsn_in_cb engine port size user-data)
          (uc_engine* unsigned-int32 int scheme-object)
          unsigned-int32 "hookinsn_in_cb" "static"
          (let ((scm-engine (car user-data))
                (callback (cadr user-data))
                (user-data0 (caddr user-data)))
            (callback scm-engine port size user-data0)))

(c-define (hookinsn_out_cb engine port size value user-data)
          (uc_engine* unsigned-int32 int unsigned-int32 scheme-object)
          unsigned-int32 "hookinsn_out_cb" "static"
          (let ((scm-engine (car user-data))
                (callback (cadr user-data))
                (user-data0 (caddr user-data)))
            (callback scm-engine port size value user-data0)))

(c-define (hookmem_cb engine type address size value user-data)
          (uc_engine* uc_mem_type unsigned-int64 int int64 scheme-object)
          void "hookmem_cb" "static"
          (let ((scm-engine (car user-data))
                (callback (cadr user-data))
                (user-data0 (caddr user-data)))
            (callback scm-engine type address size value user-data0)))

(c-define (hookeventmem_cb engine type address size value user-data)
          (uc_engine* uc_mem_type unsigned-int64 int int64 scheme-object)
          void "hookeventmem_cb" "static"
          (let ((scm-engine (car user-data))
                (callback (cadr user-data))
                (user-data0 (caddr user-data)))
            (callback scm-engine type address size value user-data0)))

(c-declare #<<END-C

static uc_engine *ffi_uc_make_engine(uc_arch arch, uc_mode mode)
{
 uc_engine *engine;
 if (uc_open(arch, mode, &engine) != UC_ERR_OK)
  return ((uc_engine*)NULL);
 else
  return engine;
}

static uc_err ffi_uc_reg_write(uc_engine *uc, int regid, uint64_t value)
{
 void *val = (void *) &value;
 return uc_reg_write(uc, regid, val);
}

static ___SCMOBJ ffi_uc_reg_read(uc_engine *uc, int regid)
{
 uint64_t val;
 uc_reg_read(uc, regid, &val);
 return val;
}

static uc_hook ffi_uc_hook_add(uc_engine *uc, int type, ___SCMOBJ user_data,
 uint64_t begin, uint64_t end, int arg1)
{
 uc_hook hook;
 uc_err err;
 void *callback;
 if (type == UC_HOOK_INTR) {
  callback = hookintr_cb;
 } else if (type == UC_HOOK_INSN) {
  if (arg1 == UC_X86_INS_IN)
   callback = hookinsn_in_cb;
  else if (arg1 == UC_X86_INS_OUT)
   callback = hookinsn_out_cb;
 } else if (type == UC_HOOK_CODE || type == UC_HOOK_BLOCK) {
  callback = hookcode_cb;
 } else if (type & (UC_HOOK_MEM_READ_UNMAPPED |  // |
                    UC_HOOK_MEM_WRITE_UNMAPPED | // |
                    UC_HOOK_MEM_FETCH_UNMAPPED | // |
                    UC_HOOK_MEM_READ_PROT |      // |
                    UC_HOOK_MEM_WRITE_PROT |     // |
                    UC_HOOK_MEM_FETCH_PROT)) {
  callback = hookeventmem_cb;
 } else {
  callback = hookmem_cb;
 }
 if ((err = uc_hook_add(uc, &hook, type, callback, user_data, begin, end, arg1)) != UC_ERR_OK)
   return -1;
 return hook;
}

static uc_err ffi_uc_mem_write(uc_engine *uc, uint64_t address, ___SCMOBJ bytes)
{
 return uc_mem_write(uc, address, U8_DATA(bytes), U8_LEN(bytes));
}

static uc_err ffi_uc_mem_read(uc_engine *uc, uint64_t address, ___SCMOBJ bytes, size_t size)
{
 return uc_mem_read(uc, address, U8_DATA(bytes), size);
}

static void ffi_uc_close(void *ptr)
{
 uc_engine *uc = (uc_engine*) ptr;
 uc_close(uc);
 return;
}

END-C
)

)