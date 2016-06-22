#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

void *map_file(const char *file, size_t *size) {
  int fd = open(file, O_RDONLY, S_IRUSR);
  if (-1 == fd) goto open;

  struct stat stat;
  if (-1 == fstat(fd, &stat)) goto stat;

  void *ptr = mmap(0, stat.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if ((void*)(-1) == ptr) goto mmap;

  if (-1 == close(fd)) goto fclose;

  *size = stat.st_size;
  return ptr;

 fclose:
  munmap(ptr, stat.st_size);
 mmap:
 stat:
  close(fd);
 open:
  return 0;
}
