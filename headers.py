standard_header_types = {
"assert.h" : [],
"complex.h" : [],
"ctype.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "__locale_t", "locale_t"],
"errno.h" : [],
"fenv.h" : ["fexcept_t", "fenv_t"],
"inttypes.h" : ["int8_t", "int16_t", "int32_t", "int64_t", "uint8_t", "uint16_t",
        "uint32_t", "uint64_t", "int_least8_t", "int_least16_t",
        "int_least32_t", "int_least64_t", "uint_least8_t", "uint_least16_t",
        "uint_least32_t", "uint_least64_t", "int_fast8_t", "int_fast16_t",
        "int_fast32_t", "int_fast64_t", "uint_fast8_t", "uint_fast16_t",
        "uint_fast32_t", "uint_fast64_t", "intptr_t", "uintptr_t", "intmax_t",
        "uintmax_t", "__gwchar_t", "imaxdiv_t"],
"limits.h" : [],
"locale.h" : ["lconv", "__locale_t", "locale_t"],
"math.h" : ["float_t", "double_t", "_LIB_VERSION_TYPE"],
"signal.h" : ["__sig_atomic_t", "__sigset_t", "sig_atomic_t", "sigset_t",
        "__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "pid_t", "uid_t", "sigval_t", "siginfo_t",
        "sigevent_t", "sig_t",
        "__s8", "__u8", "__s16", "__u16", "__s32", "__u32", "__s64", "__u64",
        "umode_t", "__kernel_fd_set", "__kernel_key_t", "__kernel_mqd_t",
        "__kernel_ino_t", "__kernel_mode_t", "__kernel_nlink_t",
        "__kernel_off_t", "__kernel_pid_t", "__kernel_ipc_pid_t",
        "__kernel_uid_t", "__kernel_gid_t", "__kernel_size_t",
        "__kernel_ssize_t", "__kernel_ptrdiff_t", "__kernel_time_t",
        "__kernel_suseconds_t", "__kernel_clock_t", "__kernel_timer_t",
        "__kernel_clockid_t", "__kernel_daddr_t", "__kernel_caddr_t",
        "__kernel_uid16_t", "__kernel_gid16_t", "__kernel_uid32_t",
        "__kernel_gid32_t", "__kernel_old_uid_t", "__kernel_old_gid_t",
        "__kernel_old_dev_t", "__kernel_loff_t", "__kernel_fsid_t", "__le16",
        "__be16", "__le32", "__be32", "__le64", "__be64", "__sum16", "__wsum",
        "size_t", "stack_t", "greg_t", "gregset_t[19]", "_libc_fpreg",
        "_libc_fpstate", "fpregset_t", "mcontext_t", "ucontext_t", "pthread_t",
        "pthread_attr_t", "__pthread_slist_t", "pthread_mutex_t",
        "pthread_mutexattr_t", "pthread_cond_t", "pthread_condattr_t",
        "pthread_key_t", "pthread_once_t", "pthread_rwlock_t",
        "pthread_rwlockattr_t", "pthread_spinlock_t", "pthread_barrier_t",
        "pthread_barrierattr_t"],
"stdint.h" : ["int8_t", "int16_t", "int32_t", "int64_t", "uint8_t", "uint16_t",
        "uint32_t", "uint64_t", "int_least8_t", "int_least16_t",
        "int_least32_t", "int_least64_t", "uint_least8_t", "uint_least16_t",
        "uint_least32_t", "uint_least64_t", "int_fast8_t", "int_fast16_t",
        "int_fast32_t", "int_fast64_t", "uint_fast8_t", "uint_fast16_t",
        "uint_fast32_t", "uint_fast64_t", "intptr_t", "uintptr_t", "intmax_t",
        "uintmax_t"],
"stdio.h" : ["size_t", "__u_char", "__u_short", "__u_int", "__u_long",
         "__int8_t", "__uint8_t", "__int16_t", "__uint16_t", "__int32_t",
         "__uint32_t", "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t",
         "__dev_t", "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t",
         "__nlink_t", "__off_t", "__off64_t", "__pid_t", "__fsid_t",
         "__clock_t", "__rlim_t", "__rlim64_t", "__id_t", "__time_t",
         "__useconds_t", "__suseconds_t", "__daddr_t", "__swblk_t", "__key_t",
         "__clockid_t", "__timer_t", "__blksize_t", "__blkcnt_t",
         "__blkcnt64_t", "__fsblkcnt_t", "__fsblkcnt64_t", "__fsfilcnt_t",
         "__fsfilcnt64_t", "__ssize_t", "__loff_t", "__qaddr_t", "__caddr_t",
         "__intptr_t", "__socklen_t", "_IO_FILE", "FILE", "__FILE",
         "__mbstate_t", "_G_fpos_t", "_G_fpos64_t", "__gnuc_va_list",
         "_IO_jump_t", "_IO_FILE", "_IO_lock_t", "__codecvt_result",
         "_IO_FILE", "_IO_FILE_plus", "va_list", "off_t", "ssize_t", "fpos_t"],
"stdlib.h" : ["size_t", "wchar_t", "div_t", "ldiv_t", "lldiv_t", "__u_char",
        "__u_short", "__u_int", "__u_long", "__int8_t", "__uint8_t",
        "__int16_t", "__uint16_t", "__int32_t", "__uint32_t", "__int64_t",
        "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t", "__uid_t",
        "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t", "__off_t",
        "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "u_char", "u_short", "u_int", "u_long", "quad_t", "u_quad_t", "fsid_t",
        "loff_t", "ino_t", "dev_t", "gid_t", "mode_t", "nlink_t", "uid_t",
        "off_t", "pid_t", "id_t", "ssize_t", "daddr_t", "caddr_t", "key_t",
        "clock_t", "time_t", "clockid_t", "timer_t", "ulong", "ushort", "uint",
        "__sig_atomic_t", "__sigset_t", "sigset_t", "suseconds_t", "__fd_mask",
        "fd_set", "fd_mask", "blksize_t", "blkcnt_t", "fsblkcnt_t",
        "fsfilcnt_t", "pthread_t", "pthread_attr_t", "__pthread_slist_t",
        "pthread_mutex_t", "pthread_mutexattr_t", "pthread_cond_t",
        "pthread_condattr_t", "pthread_key_t", "pthread_once_t",
        "pthread_rwlock_t", "pthread_rwlockattr_t", "pthread_spinlock_t",
        "pthread_barrier_t", "pthread_barrierattr_t"],
"string.h" : ["size_t", "__locale_t", "locale_t"],
"pthread.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "size_t", "time_t", "pid_t", "__cpu_mask", "cpu_set_t", "clock_t",
        "clockid_t", "timer_t", "tm", "sigevent", "__locale_t", "locale_t",
        "pthread_t", "pthread_attr_t", "__pthread_slist_t", "pthread_mutex_t",
        "pthread_mutexattr_t", "pthread_cond_t", "pthread_condattr_t",
        "pthread_key_t", "pthread_once_t", "pthread_rwlock_t",
        "pthread_rwlockattr_t", "pthread_spinlock_t", "pthread_barrier_t",
        "pthread_barrierattr_t", "__jmp_buf[6]", "_pthread_cleanup_buffer",
        "__pthread_cleanup_frame", "__jmp_buf_tag"],
"tgmath.h" : ["float_t", "double_t", "_LIB_VERSION_TYPE"],
"time.h" : ["size_t", "__u_char", "__u_short", "__u_int", "__u_long",
        "__int8_t", "__uint8_t", "__int16_t", "__uint16_t", "__int32_t",
        "__uint32_t", "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t",
        "__dev_t", "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t",
        "__nlink_t", "__off_t", "__off64_t", "__pid_t", "__fsid_t",
        "__clock_t", "__rlim_t", "__rlim64_t", "__id_t", "__time_t",
        "__useconds_t", "__suseconds_t", "__daddr_t", "__swblk_t", "__key_t",
        "__clockid_t", "__timer_t", "__blksize_t", "__blkcnt_t",
        "__blkcnt64_t", "__fsblkcnt_t", "__fsblkcnt64_t", "__fsfilcnt_t",
        "__fsfilcnt64_t", "__ssize_t", "__loff_t", "__qaddr_t", "__caddr_t",
        "__intptr_t", "__socklen_t", "clock_t", "time_t", "clockid_t",
        "timer_t", "tm", "sigevent", "pid_t", "__locale_t", "locale_t"],
"wchar.h" : ["_IO_FILE", "FILE", "__FILE", "__gnuc_va_list", "size_t",
        "wchar_t", "wint_t", "__mbstate_t", "mbstate_t", "tm", "__locale_t",
        "locale_t"],
"wctype.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "wint_t", "wctype_t", "wctrans_t", "__locale_t", "locale_t"],
"unistd.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "ssize_t", "size_t", "gid_t", "uid_t", "off_t", "useconds_t", "pid_t",
        "intptr_t", "socklen_t"],
"netdb.h" : ["int8_t", "int16_t", "int32_t", "int64_t", "uint8_t", "uint16_t",
        "uint32_t", "uint64_t", "int_least8_t", "int_least16_t",
        "int_least32_t", "int_least64_t", "uint_least8_t", "uint_least16_t",
        "uint_least32_t", "uint_least64_t", "int_fast8_t", "int_fast16_t",
        "int_fast32_t", "int_fast64_t", "uint_fast8_t", "uint_fast16_t",
        "uint_fast32_t", "uint_fast64_t", "intptr_t", "uintptr_t", "intmax_t",
        "uintmax_t", "__u_char", "__u_short", "__u_int", "__u_long",
        "__int8_t", "__uint8_t", "__int16_t", "__uint16_t", "__int32_t",
        "__uint32_t", "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t",
        "__dev_t", "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t",
        "__nlink_t", "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t",
        "__rlim_t", "__rlim64_t", "__id_t", "__time_t", "__useconds_t",
        "__suseconds_t", "__daddr_t", "__swblk_t", "__key_t", "__clockid_t",
        "__timer_t", "__blksize_t", "__blkcnt_t", "__blkcnt64_t",
        "__fsblkcnt_t", "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t",
        "__ssize_t", "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t",
        "__socklen_t", "u_char", "u_short", "u_int", "u_long", "quad_t",
        "u_quad_t", "fsid_t", "loff_t", "ino_t", "dev_t", "gid_t", "mode_t",
        "nlink_t", "uid_t", "off_t", "pid_t", "id_t", "ssize_t", "daddr_t",
        "caddr_t", "key_t", "clock_t", "time_t", "clockid_t", "timer_t",
        "size_t", "ulong", "ushort", "uint", "__sig_atomic_t", "__sigset_t",
        "sigset_t", "suseconds_t", "__fd_mask", "fd_set", "fd_mask",
        "blksize_t", "blkcnt_t", "fsblkcnt_t", "fsfilcnt_t", "pthread_t",
        "pthread_attr_t", "__pthread_slist_t", "pthread_mutex_t",
        "pthread_mutexattr_t", "pthread_cond_t", "pthread_condattr_t",
        "pthread_key_t", "pthread_once_t", "pthread_rwlock_t",
        "pthread_rwlockattr_t", "pthread_spinlock_t", "pthread_barrier_t",
        "pthread_barrierattr_t", "socklen_t", "__socket_type", "sa_family_t",
        "in_port_t", "in_addr_t", "rpcent", "netent", "hostent", "servent",
        "protoent", "addrinfo"],
"sys/types.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "u_char", "u_short", "u_int", "u_long", "quad_t", "u_quad_t", "fsid_t",
        "loff_t", "ino_t", "dev_t", "gid_t", "mode_t", "nlink_t", "uid_t",
        "off_t", "pid_t", "id_t", "ssize_t", "daddr_t", "caddr_t", "key_t",
        "clock_t", "time_t", "clockid_t", "timer_t", "size_t", "ulong",
        "ushort", "uint", "__sig_atomic_t", "__sigset_t", "sigset_t",
        "suseconds_t", "__fd_mask", "fd_set", "fd_mask", "blksize_t",
        "blkcnt_t", "fsblkcnt_t", "fsfilcnt_t", "pthread_t", "pthread_attr_t",
        "__pthread_slist_t", "pthread_mutex_t", "pthread_mutexattr_t",
        "pthread_cond_t", "pthread_condattr_t", "pthread_key_t",
        "pthread_once_t", "pthread_rwlock_t", "pthread_rwlockattr_t",
        "pthread_spinlock_t", "pthread_barrier_t", "pthread_barrierattr_t"],
"sys/socket.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "u_char", "u_short", "u_int", "u_long", "quad_t", "u_quad_t", "fsid_t",
        "loff_t", "ino_t", "dev_t", "gid_t", "mode_t", "nlink_t", "uid_t",
        "off_t", "pid_t", "id_t", "ssize_t", "daddr_t", "caddr_t", "key_t",
        "clock_t", "time_t", "clockid_t", "timer_t", "size_t", "ulong",
        "ushort", "uint", "__sig_atomic_t", "__sigset_t", "sigset_t",
        "suseconds_t", "__fd_mask", "fd_set", "fd_mask", "blksize_t",
        "blkcnt_t", "fsblkcnt_t", "fsfilcnt_t", "pthread_t", "pthread_attr_t",
        "__pthread_slist_t", "pthread_mutex_t", "pthread_mutexattr_t",
        "pthread_cond_t", "pthread_condattr_t", "pthread_key_t",
        "pthread_once_t", "pthread_rwlock_t", "pthread_rwlockattr_t",
        "pthread_spinlock_t", "pthread_barrier_t", "pthread_barrierattr_t",
        "socklen_t", "__socket_type", "sa_family_t"],
"netinet/in.h" : ["int8_t", "int16_t", "int32_t", "int64_t", "uint8_t",
        "uint16_t", "uint32_t", "uint64_t", "int_least8_t", "int_least16_t",
        "int_least32_t", "int_least64_t", "uint_least8_t", "uint_least16_t",
        "uint_least32_t", "uint_least64_t", "int_fast8_t", "int_fast16_t",
        "int_fast32_t", "int_fast64_t", "uint_fast8_t", "uint_fast16_t",
        "uint_fast32_t", "uint_fast64_t", "intptr_t", "uintptr_t", "intmax_t",
        "uintmax_t", "__u_char", "__u_short", "__u_int", "__u_long",
        "__int8_t", "__uint8_t", "__int16_t", "__uint16_t", "__int32_t",
        "__uint32_t", "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t",
        "__dev_t", "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t",
        "__nlink_t", "__off_t", "__off64_t", "__pid_t", "__fsid_t",
        "__clock_t", "__rlim_t", "__rlim64_t", "__id_t", "__time_t",
        "__useconds_t", "__suseconds_t", "__daddr_t", "__swblk_t", "__key_t",
        "__clockid_t", "__timer_t", "__blksize_t", "__blkcnt_t",
        "__blkcnt64_t", "__fsblkcnt_t", "__fsblkcnt64_t", "__fsfilcnt_t",
        "__fsfilcnt64_t", "__ssize_t", "__loff_t", "__qaddr_t", "__caddr_t",
        "__intptr_t", "__socklen_t", "u_char", "u_short", "u_int", "u_long",
        "quad_t", "u_quad_t", "fsid_t", "loff_t", "ino_t", "dev_t", "gid_t",
        "mode_t", "nlink_t", "uid_t", "off_t", "pid_t", "id_t", "ssize_t",
        "daddr_t", "caddr_t", "key_t", "clock_t", "time_t", "clockid_t",
        "timer_t", "size_t", "ulong", "ushort", "uint", "__sig_atomic_t",
        "__sigset_t", "sigset_t", "suseconds_t", "__fd_mask", "fd_set",
        "fd_mask", "blksize_t", "blkcnt_t", "fsblkcnt_t", "fsfilcnt_t",
        "pthread_t", "pthread_attr_t", "__pthread_slist_t", "pthread_mutex_t",
        "pthread_mutexattr_t", "pthread_cond_t", "pthread_condattr_t",
        "pthread_key_t", "pthread_once_t", "pthread_rwlock_t",
        "pthread_rwlockattr_t", "pthread_spinlock_t", "pthread_barrier_t",
        "pthread_barrierattr_t", "socklen_t", "__socket_type", "sa_family_t",
        "in_port_t", "in_addr_t"],
"arpa/inet.h" : ["int8_t", "int16_t", "int32_t", "int64_t", "uint8_t",
        "uint16_t", "uint32_t", "uint64_t", "int_least8_t", "int_least16_t",
        "int_least32_t", "int_least64_t", "uint_least8_t", "uint_least16_t",
        "uint_least32_t", "uint_least64_t", "int_fast8_t", "int_fast16_t",
        "int_fast32_t", "int_fast64_t", "uint_fast8_t", "uint_fast16_t",
        "uint_fast32_t", "uint_fast64_t", "intptr_t", "uintptr_t", "intmax_t",
        "uintmax_t", "__u_char", "__u_short", "__u_int", "__u_long",
        "__int8_t", "__uint8_t", "__int16_t", "__uint16_t", "__int32_t",
        "__uint32_t", "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t",
        "__dev_t", "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t",
        "__nlink_t", "__off_t", "__off64_t", "__pid_t", "__fsid_t",
        "__clock_t", "__rlim_t", "__rlim64_t", "__id_t", "__time_t",
        "__useconds_t", "__suseconds_t", "__daddr_t", "__swblk_t", "__key_t",
        "__clockid_t", "__timer_t", "__blksize_t", "__blkcnt_t",
        "__blkcnt64_t", "__fsblkcnt_t", "__fsblkcnt64_t", "__fsfilcnt_t",
        "__fsfilcnt64_t", "__ssize_t", "__loff_t", "__qaddr_t", "__caddr_t",
        "__intptr_t", "__socklen_t", "u_char", "u_short", "u_int", "u_long",
        "quad_t", "u_quad_t", "fsid_t", "loff_t", "ino_t", "dev_t", "gid_t",
        "mode_t", "nlink_t", "uid_t", "off_t", "pid_t", "id_t", "ssize_t",
        "daddr_t", "caddr_t", "key_t", "clock_t", "time_t", "clockid_t",
        "timer_t", "size_t", "ulong", "ushort", "uint", "__sig_atomic_t",
        "__sigset_t", "sigset_t", "suseconds_t", "__fd_mask", "fd_set",
        "fd_mask", "blksize_t", "blkcnt_t", "fsblkcnt_t", "fsfilcnt_t",
        "pthread_t", "pthread_attr_t", "__pthread_slist_t", "pthread_mutex_t",
        "pthread_mutexattr_t", "pthread_cond_t", "pthread_condattr_t",
        "pthread_key_t", "pthread_once_t", "pthread_rwlock_t",
        "pthread_rwlockattr_t", "pthread_spinlock_t", "pthread_barrier_t",
        "pthread_barrierattr_t", "socklen_t", "__socket_type", "sa_family_t",
        "in_port_t", "in_addr_t"],
"sys/wait.h" : ["__sig_atomic_t", "__sigset_t", "sig_atomic_t", "sigset_t",
        "__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "pid_t", "uid_t", "sigval_t", "siginfo_t", "sigevent_t", "sig_t",
        "__s8", "__u8", "__s16", "__u16", "__s32", "__u32", "__s64", "__u64",
        "umode_t", "__kernel_fd_set", "__kernel_key_t", "__kernel_mqd_t",
        "__kernel_ino_t", "__kernel_mode_t", "__kernel_nlink_t",
        "__kernel_off_t", "__kernel_pid_t", "__kernel_ipc_pid_t",
        "__kernel_uid_t", "__kernel_gid_t", "__kernel_size_t",
        "__kernel_ssize_t", "__kernel_ptrdiff_t", "__kernel_time_t",
        "__kernel_suseconds_t", "__kernel_clock_t", "__kernel_timer_t",
        "__kernel_clockid_t", "__kernel_daddr_t", "__kernel_caddr_t",
        "__kernel_uid16_t", "__kernel_gid16_t", "__kernel_uid32_t",
        "__kernel_gid32_t", "__kernel_old_uid_t", "__kernel_old_gid_t",
        "__kernel_old_dev_t", "__kernel_loff_t", "__kernel_fsid_t", "__le16",
        "__be16", "__le32", "__be32", "__le64", "__be64", "__sum16", "__wsum",
        "size_t", "stack_t", "greg_t", "gregset_t[19]", "_libc_fpreg",
        "_libc_fpstate", "fpregset_t", "mcontext_t", "ucontext_t", "pthread_t",
        "pthread_attr_t", "__pthread_slist_t", "pthread_mutex_t",
        "pthread_mutexattr_t", "pthread_cond_t", "pthread_condattr_t",
        "pthread_key_t", "pthread_once_t", "pthread_rwlock_t",
        "pthread_rwlockattr_t", "pthread_spinlock_t", "pthread_barrier_t",
        "pthread_barrierattr_t", "__rlimit_resource", "rlim_t", "__rusage_who",
        "__priority_which", "id_t", "__rlimit_resource_t", "__rusage_who_t",
        "__priority_which_t", "idtype_t", "rusage"],
"fcntl.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "u_char", "u_short", "u_int", "u_long", "quad_t", "u_quad_t", "fsid_t",
        "loff_t", "ino_t", "dev_t", "gid_t", "mode_t", "nlink_t", "uid_t",
        "off_t", "pid_t", "id_t", "ssize_t", "daddr_t", "caddr_t", "key_t",
        "clock_t", "time_t", "clockid_t", "timer_t", "size_t", "ulong",
        "ushort", "uint", "__sig_atomic_t", "__sigset_t", "sigset_t",
        "suseconds_t", "__fd_mask", "fd_set", "fd_mask", "blksize_t",
        "blkcnt_t", "fsblkcnt_t", "fsfilcnt_t", "pthread_t", "pthread_attr_t",
        "__pthread_slist_t", "pthread_mutex_t", "pthread_mutexattr_t",
        "pthread_cond_t", "pthread_condattr_t", "pthread_key_t",
        "pthread_once_t", "pthread_rwlock_t", "pthread_rwlockattr_t",
        "pthread_spinlock_t", "pthread_barrier_t", "pthread_barrierattr_t"],
"semaphore.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "u_char", "u_short", "u_int", "u_long", "quad_t", "u_quad_t", "fsid_t",
        "loff_t", "ino_t", "dev_t", "gid_t", "mode_t", "nlink_t", "uid_t",
        "off_t", "pid_t", "id_t", "ssize_t", "daddr_t", "caddr_t", "key_t",
        "clock_t", "time_t", "clockid_t", "timer_t", "size_t", "ulong",
        "ushort", "uint", "__sig_atomic_t", "__sigset_t", "sigset_t",
        "suseconds_t", "__fd_mask", "fd_set", "fd_mask", "blksize_t",
        "blkcnt_t", "fsblkcnt_t", "fsfilcnt_t", "pthread_t", "pthread_attr_t",
        "__pthread_slist_t", "pthread_mutex_t", "pthread_mutexattr_t",
        "pthread_cond_t", "pthread_condattr_t", "pthread_key_t",
        "pthread_once_t", "pthread_rwlock_t", "pthread_rwlockattr_t",
        "pthread_spinlock_t", "pthread_barrier_t", "pthread_barrierattr_t",
        "sem_t"],
"stdbool.h" : ["bool", "_Bool"],
"sys/signal.h" : ["__sig_atomic_t", "__sigset_t", "sig_atomic_t", "sigset_t",
        "__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "pid_t", "uid_t", "sigval_t", "siginfo_t", "sigevent_t", "sig_t",
        "__s8", "__u8", "__s16", "__u16", "__s32", "__u32", "__s64", "__u64",
        "umode_t", "__kernel_fd_set", "__kernel_key_t", "__kernel_mqd_t",
        "__kernel_ino_t", "__kernel_mode_t", "__kernel_nlink_t",
        "__kernel_off_t", "__kernel_pid_t", "__kernel_ipc_pid_t",
        "__kernel_uid_t", "__kernel_gid_t", "__kernel_size_t",
        "__kernel_ssize_t", "__kernel_ptrdiff_t", "__kernel_time_t",
        "__kernel_suseconds_t", "__kernel_clock_t", "__kernel_timer_t",
        "__kernel_clockid_t", "__kernel_daddr_t", "__kernel_caddr_t",
        "__kernel_uid16_t", "__kernel_gid16_t", "__kernel_uid32_t",
        "__kernel_gid32_t", "__kernel_old_uid_t", "__kernel_old_gid_t",
        "__kernel_old_dev_t", "__kernel_loff_t", "__kernel_fsid_t", "__le16",
        "__be16", "__le32", "__be32", "__le64", "__be64", "__sum16", "__wsum",
        "size_t", "stack_t", "greg_t", "gregset_t[19]", "_libc_fpreg",
        "_libc_fpstate", "fpregset_t", "mcontext_t", "ucontext_t", "pthread_t",
        "pthread_attr_t", "__pthread_slist_t", "pthread_mutex_t",
        "pthread_mutexattr_t", "pthread_cond_t", "pthread_condattr_t",
        "pthread_key_t", "pthread_once_t", "pthread_rwlock_t",
        "pthread_rwlockattr_t", "pthread_spinlock_t", "pthread_barrier_t",
        "pthread_barrierattr_t"],
"sys/uio.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "u_char", "u_short", "u_int", "u_long", "quad_t", "u_quad_t", "fsid_t",
        "loff_t", "ino_t", "dev_t", "gid_t", "mode_t", "nlink_t", "uid_t",
        "off_t", "pid_t", "id_t", "ssize_t", "daddr_t", "caddr_t", "key_t",
        "clock_t", "time_t", "clockid_t", "timer_t", "size_t", "ulong",
        "ushort", "uint", "__sig_atomic_t", "__sigset_t", "sigset_t",
        "suseconds_t", "__fd_mask", "fd_set", "fd_mask", "blksize_t",
        "blkcnt_t", "fsblkcnt_t", "fsfilcnt_t", "pthread_t", "pthread_attr_t",
        "__pthread_slist_t", "pthread_mutex_t", "pthread_mutexattr_t",
        "pthread_cond_t", "pthread_condattr_t", "pthread_key_t",
        "pthread_once_t", "pthread_rwlock_t", "pthread_rwlockattr_t",
        "pthread_spinlock_t", "pthread_barrier_t", "pthread_barrierattr_t"],
"sys/time.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "time_t", "__sig_atomic_t", "__sigset_t", "sigset_t", "suseconds_t",
        "__fd_mask", "fd_set", "fd_mask", "__timezone_ptr_t",
        "__itimer_which_t"],
"wait.h" : ["__sig_atomic_t", "__sigset_t", "sig_atomic_t", "sigset_t",
        "__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "pid_t", "uid_t", "sigval_t", "siginfo_t", "sigevent_t", "sig_t",
        "__s8", "__u8", "__s16", "__u16", "__s32", "__u32", "__s64", "__u64",
        "umode_t", "__kernel_fd_set", "__kernel_key_t", "__kernel_mqd_t",
        "__kernel_ino_t", "__kernel_mode_t", "__kernel_nlink_t",
        "__kernel_off_t", "__kernel_pid_t", "__kernel_ipc_pid_t",
        "__kernel_uid_t", "__kernel_gid_t", "__kernel_size_t",
        "__kernel_ssize_t", "__kernel_ptrdiff_t", "__kernel_time_t",
        "__kernel_suseconds_t", "__kernel_clock_t", "__kernel_timer_t",
        "__kernel_clockid_t", "__kernel_daddr_t", "__kernel_caddr_t",
        "__kernel_uid16_t", "__kernel_gid16_t", "__kernel_uid32_t",
        "__kernel_gid32_t", "__kernel_old_uid_t", "__kernel_old_gid_t",
        "__kernel_old_dev_t", "__kernel_loff_t", "__kernel_fsid_t", "__le16",
        "__be16", "__le32", "__be32", "__le64", "__be64", "__sum16", "__wsum",
        "size_t", "stack_t", "greg_t", "gregset_t[19]", "_libc_fpreg",
        "_libc_fpstate", "fpregset_t", "mcontext_t", "ucontext_t", "pthread_t",
        "pthread_attr_t", "__pthread_slist_t", "pthread_mutex_t",
        "pthread_mutexattr_t", "pthread_cond_t", "pthread_condattr_t",
        "pthread_key_t", "pthread_once_t", "pthread_rwlock_t",
        "pthread_rwlockattr_t", "pthread_spinlock_t", "pthread_barrier_t",
        "pthread_barrierattr_t", "__rlimit_resource", "rlim_t", "__rusage_who",
        "__priority_which", "id_t", "__rlimit_resource_t", "__rusage_who_t",
        "__priority_which_t", "idtype_t", "rusage"],
"sys/select.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "__sig_atomic_t", "__sigset_t", "sigset_t", "time_t", "suseconds_t",
        "__fd_mask", "fd_set", "fd_mask"],
"poll.h" : ["nfds_t"],
"sys/poll.h" : ["nfds_t"],
"sys/stat.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "time_t", "dev_t", "gid_t", "ino_t", "mode_t", "nlink_t", "off_t",
        "uid_t"],
"stddef.h" : ["ptrdiff_t", "size_t", "wchar_t"],
"sys/ipc.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "__ipc_pid_t", "uid_t", "gid_t", "mode_t", "key_t"],
"sys/sem.h" : [],
"regex.h" : ["__u_char", "__u_short", "__u_int", "__u_long", "__int8_t",
        "__uint8_t", "__int16_t", "__uint16_t", "__int32_t", "__uint32_t",
        "__int64_t", "__uint64_t", "__quad_t", "__u_quad_t", "__dev_t",
        "__uid_t", "__gid_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
        "__off_t", "__off64_t", "__pid_t", "__fsid_t", "__clock_t", "__rlim_t",
        "__rlim64_t", "__id_t", "__time_t", "__useconds_t", "__suseconds_t",
        "__daddr_t", "__swblk_t", "__key_t", "__clockid_t", "__timer_t",
        "__blksize_t", "__blkcnt_t", "__blkcnt64_t", "__fsblkcnt_t",
        "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t", "__ssize_t",
        "__loff_t", "__qaddr_t", "__caddr_t", "__intptr_t", "__socklen_t",
        "u_char", "u_short", "u_int", "u_long", "quad_t", "u_quad_t", "fsid_t",
        "loff_t", "ino_t", "dev_t", "gid_t", "mode_t", "nlink_t", "uid_t",
        "off_t", "pid_t", "id_t", "ssize_t", "daddr_t", "caddr_t", "key_t",
        "clock_t", "time_t", "clockid_t", "timer_t", "size_t", "ulong",
        "ushort", "uint", "__sig_atomic_t", "__sigset_t", "sigset_t",
        "suseconds_t", "__fd_mask", "fd_set", "fd_mask", "blksize_t",
        "blkcnt_t", "fsblkcnt_t", "fsfilcnt_t", "pthread_t", "pthread_attr_t",
        "__pthread_list_t", "pthread_mutex_t", "pthread_mutexattr_t",
        "pthread_cond_t", "pthread_condattr_t", "pthread_key_t",
        "pthread_once_t", "pthread_rwlock_t", "pthread_rwlockattr_t",
        "pthread_spinlock_t", "pthread_barrier_t", "pthread_barrierattr_t",
        "s_reg_t", "active_reg_t", "reg_syntax_t", "reg_errcode_t",
        "re_pattern_buffer", "regex_t", "regoff_t", "regmatch_t"]
}


