# Markdown 阅读示例

Mermaid 图表会直接显示在正文中。将光标移到图表上，按 **C-c C-x m** 查看源码，再按一次返回图形。

## 从文档到图形

```mermaid
flowchart LR
    A["打开 Markdown"] --> B{"发现 Mermaid 代码块"}
    B --> C["Pele 生成 SVG"]
    C --> D["在正文中阅读图表"]
```

正文和图表保存在同一份 Markdown 中，复制、保存时仍然使用原始文本。

## 阅读与修改

```mermaid
sequenceDiagram
    participant 用户
    participant Emacs
    participant Pele
    用户->>Emacs: 打开文档
    Emacs->>Pele: 批量发送 Mermaid 源码
    Pele-->>Emacs: 返回 SVG
    Emacs-->>用户: 显示内联图表
```

- **C-c C-x m**：切换当前图表的源码与图片。
- **C-c C-x M**：切换整篇文档的图表。
- **C-c C-x r**：刷新图表；加 **C-u** 可清除缓存并重试。
- **C-c C-x +** 或 **C-c C-x =**：放大当前图表。
- **C-c C-x -**：缩小当前图表。
- **C-c C-x 0**：恢复适应窗口的大小。
- **M-x frei/markdown-mermaid-mode**：开启或关闭 Mermaid 显示。

放大后可用 **C-x <**、**C-x >** 横向滚动查看。缩放比例会在刷新和切换源码后保留。

## 文档中的代码示例

下面的围栏包含一个 Markdown 代码示例，其中的 Mermaid 保持为文本：

````markdown
```mermaid
flowchart TD
    A --> B
```
````
