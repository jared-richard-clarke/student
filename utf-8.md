# Unicode Transformation Format (UTF-8)

UTF-8 uses 1, 2, 3, or 4 bytes depending on the code point.

| byte 1   | byte 2   | byte 3   | byte 4   | available bits |
| -------- | -------- | -------- | -------- | -------------- |
| 0xxxxxxx |          |          |          | 7              |
| 110xxxxx | 10xxxxxx |          |          | 11             |
| 1110xxxx | 10xxxxxx | 10xxxxxx |          | 16             |
| 11110xxx | 10xxxxxx | 10xxxxxx | 10xxxxxx | 21             |
