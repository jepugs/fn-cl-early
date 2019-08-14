AC_INIT([fn],[prerelease])

AC_ARG_VAR([SBCL],[SBCL executable path])
AC_PATH_PROG([SBCL],[sbcl])
AS_IF([test -z "$SBCL"], [AC_MSG_ERROR([SBCL executable not found])])

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
