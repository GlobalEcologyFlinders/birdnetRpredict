#!/usr/bin/env python3

import os
import shutil
import subprocess
import sys
import tarfile
import time


def emit(event, *fields):
    sys.stdout.write("\t".join([event, *[str(field) for field in fields]]) + "\n")
    sys.stdout.flush()


def is_real_audio_member(member):
    name = member.name
    base_name = os.path.basename(name)

    if not member.isfile():
        return False

    if not name.lower().endswith(".flac"):
        return False

    if base_name.startswith("._"):
        return False

    if "/__MACOSX/" in name or name.startswith("__MACOSX/"):
        return False

    return True


def main():
    if len(sys.argv) < 3:
      raise SystemExit("usage: stream_archive_flacs.py <archive_path> <extract_root> [heartbeat_seconds]")

    archive_path = sys.argv[1]
    extract_root = sys.argv[2]
    heartbeat_seconds = float(sys.argv[3]) if len(sys.argv) > 3 else 5.0

    members_seen = 0
    flac_found = 0
    last_progress_at = time.monotonic()

    zstd_proc = subprocess.Popen(
        ["zstd", "-dc", archive_path],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=False,
    )

    try:
        with tarfile.open(fileobj=zstd_proc.stdout, mode="r|") as archive:
            for member in archive:
                members_seen += 1
                now = time.monotonic()

                if now - last_progress_at >= heartbeat_seconds:
                    emit("SCAN", members_seen, flac_found, member.name)
                    last_progress_at = now

                if not is_real_audio_member(member):
                    continue

                flac_found += 1
                target_path = os.path.join(extract_root, member.name)
                os.makedirs(os.path.dirname(target_path), exist_ok=True)

                source = archive.extractfile(member)
                if source is None:
                    raise RuntimeError(f"unable to extract member stream for {member.name}")

                with open(target_path, "wb") as destination:
                    shutil.copyfileobj(source, destination, length=1024 * 1024)

                emit("FILE", members_seen, flac_found, member.name, target_path)
                command = sys.stdin.readline()
                if not command:
                    break

                command = command.strip().upper()
                if command == "STOP":
                    break

                last_progress_at = time.monotonic()

        emit("COMPLETE", members_seen, flac_found, "")
    except Exception as error:
        emit("ERROR", members_seen, flac_found, str(error))
        raise
    finally:
        if zstd_proc.stdout is not None:
            zstd_proc.stdout.close()

        stderr_output = b""
        if zstd_proc.stderr is not None:
            stderr_output = zstd_proc.stderr.read()
            zstd_proc.stderr.close()

        return_code = zstd_proc.wait()
        if return_code != 0:
            message = stderr_output.decode("utf-8", errors="replace").strip()
            if message:
                emit("ERROR", members_seen, flac_found, f"zstd failed: {message}")


if __name__ == "__main__":
    main()
