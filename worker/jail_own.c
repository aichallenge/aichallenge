// Change ownership of jail chroot files between the contest user and jail user
// 
// Expects CONTEST_UID, CONTEST_GID and JAIL_GID to be defined during 
// compilation

#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <pwd.h>

#define EXIT_WRONG_ARGS 1
#define EXIT_BAD_OWNER 2
#define EXIT_BAD_JAIL 3
#define EXIT_CHOWN_FAIL 4
#define EXIT_LONG_STR 5
#define EXIT_DIR_ERROR 6

#define MAX_PATH 255
#define MAX_USER 12

int own_dir_tree(const char *base_dir, uid_t owner, gid_t group) {
    int error = 0;
    DIR *dir = opendir(base_dir);
    if (dir == NULL) {
        fprintf(stderr, "Error trying to open directory %s\n", base_dir);
        exit(EXIT_DIR_ERROR);
    }
    struct dirent *entry;
    while (entry = readdir(dir)) {
        if (strcmp(entry->d_name, ".") == 0
                || strcmp(entry->d_name, "..") == 0) {
            continue;
        }
        char child_path[MAX_PATH];
        if (snprintf(child_path, MAX_PATH, "%s/%s",
                    base_dir, entry->d_name) >= MAX_PATH) {
            fprintf(stderr, "Path within jail too long %s/%s", base_dir,
                    entry->d_name);
            exit(EXIT_LONG_STR);
        }
        if (chown(child_path, owner, group)) {
            fprintf(stderr, "Could not change owner on '%s'\n", child_path);
            error = 1;
        } else {
            // Uncomment for verbose output on every file ownership change
            // printf("Changed owner on: %s\n", child_path);
        }
        if (entry->d_type == DT_DIR) {
            error = own_dir_tree(child_path, owner, group) ? 1 : error;
        }
    }
    return error;
}

int main(int argc, char *argv[]) {
    if (argc != 3 || strlen(argv[1]) != 1) {
        fprintf(stderr, "%s <c or j> <jail number>\n", argv[0]);
        exit(EXIT_WRONG_ARGS);
    }

    char new_owner = argv[1][0];
    if (!(new_owner == 'c' || new_owner == 'j') || argv[1][1] != '\0') {
        fprintf(stderr, "First argument must be owner character 'c' or 'j'.\n");
        exit(EXIT_BAD_OWNER);
    }

    char *convend;
    int jail = strtol(argv[2], &convend, 10);
    if (*convend != '\0') {
        fprintf(stderr, "Second argument should be jail number.\n");
        exit(EXIT_BAD_JAIL);
    }

    uid_t owner_uid;
    gid_t owner_gid;
    char *path_fmt;
    if (new_owner == 'c') {
        path_fmt = "/srv/chroot/jailuser%d/scratch";
        owner_uid = CONTEST_UID;
        owner_gid = CONTEST_GID;
    } else if (new_owner == 'j') {
        path_fmt = "/srv/chroot/jailuser%d/root/home/jailuser";
        char jail_user[MAX_USER];
        if (snprintf(jail_user, MAX_USER, "jailuser%d", jail) >= MAX_USER) {
            fprintf(stderr, "Jail username too long\n");
            exit(EXIT_LONG_STR);
        }
        struct passwd *pwd = getpwnam(jail_user);
        if (pwd == NULL) {
            fprintf(stderr, "Could not get jail user id\n");
            exit(EXIT_BAD_JAIL);
        }
        owner_uid = pwd->pw_uid;
        owner_gid = JAIL_GID;
    } else {
        fprintf(stderr, "Illegal owner specifier\n");
        exit(EXIT_BAD_OWNER);
    }
    char base_path[MAX_PATH];
    int actual_len = snprintf(base_path, MAX_PATH, path_fmt, jail);
    if (actual_len >= MAX_PATH) {
        fprintf(stderr, "Path to jail too long\n");
        exit(EXIT_LONG_STR);
    }

    int status = 0;
    printf("Changing ownership to uid %d, gid %d\n", owner_uid, owner_gid);
    if (chown(base_path, owner_uid, owner_gid)) {
        fprintf(stderr, "Could not change owner on %s\n", base_path);
        status = EXIT_CHOWN_FAIL;
    } else {
        printf("Changed owner on: %s\n", base_path);
    }
    if (own_dir_tree(base_path, owner_uid, owner_gid)) {
        status = EXIT_CHOWN_FAIL;
    }

    return status;
}
