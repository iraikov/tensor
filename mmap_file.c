#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

void *mmap_file(const char *file, size_t *size) 
{
  int fd; void *ptr;
  struct stat stat;

  if ((fd = open(file, O_RDONLY, S_IRUSR)) == -1) goto openerr;

  if (fstat(fd, &stat) == -1) goto staterr;

  if ((ptr = mmap(NULL, stat.st_size, PROT_READ, MAP_SHARED, fd, 0)) == (void *)-1)
    goto mmaperr;

  if (close(fd) == -1) goto fcloseerr;

  *size = stat.st_size;

  return ptr;

 fcloseerr:
  munmap(ptr, stat.st_size);
 mmaperr:
 staterr:
  close(fd);
 openerr:
  return NULL;
}
