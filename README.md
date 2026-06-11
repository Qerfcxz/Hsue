# Hsue 🧊

**[English](#english) | [中文](#中文)**

---

<a id="english"></a>
# Hsue (English)

> A hardcore, purely functional 2D UI engine built from scratch with Haskell and the modern SDL3 GPU API.

⚠️ **WARNING: HIGHLY EXPERIMENTAL & RAPID ITERATION PHASE** ⚠️
> **Hsue is currently under heavy development.** The architecture and APIs are changing rapidly on a daily basis. **Stability is NOT guaranteed at this stage**, and backwards compatibility is not a priority. Expect crashes, panics, and breaking changes. Use at your own risk!

## 🚀 Overview

Hsue is an exploration of how to build a high-performance, modern UI framework using purely functional paradigms. Instead of relying on legacy OpenGL or Cairo, Hsue directly embraces the emerging **SDL3 GPU API** (abstracting Vulkan/D3D12/Metal) and loads pre-compiled DXIL shaders to explore the potential of hardware-accelerated UI rendering.

## ✨ Key Features

*   **Modern GPU Rendering:** Built on top of the SDL3 GPU pipeline. Features basic draw call batching and explicit GPU buffer management.
*   **Deferred Request System:** Side effects and state mutations are explicitly isolated. Widgets emit `Request`s (e.g., `Render`, `Create_widget`) to a central queue, which the engine processes sequentially, maintaining a clear functional data flow.
*   **"Projection" Caching System:** A snapshot mechanism for the UI tree (`Projection a = Without object | With object image`). It caches the visual results of hierarchical transforms (such as matrices), reducing redundant calculations and traversals during rendering.
*   **Dynamic Texture Atlas:** Includes a built-in, BSP-based texture packer. Small images are packed into dynamic atlases on the fly to reduce texture binding overhead.
*   **Active vs. Inactive Segregation:** The UI tree is structurally divided into `Active` nodes (handling logic, triggers, events) and `Inactive` nodes (pure visual collectors). This design avoids unnecessary hit-testing and traversals for static UI elements.

---

<a id="中文"></a>
# Hsue (中文)

> 一个使用 Haskell 与最新 SDL3 GPU API 从零构建的硬核、纯函数式 2D UI 引擎。

⚠️ **警告：极度实验性 & 快速迭代中** ⚠️
> **Hsue 目前正处于高频更新的快速迭代阶段。** 引擎架构和底层 API 随时可能发生破坏性更改。**目前暂不考虑任何稳定性和向后兼容性**，程序可能会出现崩溃或未定义行为。请绝对不要用于生产环境！

## 🚀 简介

Hsue 旨在探索如何用纯函数式编程的理念，构建一个现代、高性能的 UI 框架。Hsue 抛弃了老旧的 OpenGL 或 Cairo 后端，直接拥抱新兴的 **SDL3 GPU API**（对齐 Vulkan/D3D12/Metal 等图形管线），并配合预编译的 DXIL 着色器，以探索硬件加速 UI 渲染的潜力。

## ✨ 核心特性

*   **现代 GPU 渲染管线:** 基于 SDL3 GPU 接口构建。底层包含了基础的 Draw Call 合批逻辑和显式的 GPU Buffer 管理。
*   **延迟请求队列 (Request System):** 明确分离了副作用与状态变更。组件不直接执行副作用，而是向引擎发出 `Request` 意图（如渲染、创建窗口、定时器），引擎在主循环中统一消费队列，保持清晰的数据流。
*   **“投影”缓存系统 (Projection):** 一套 UI 树快照机制。通过区分本体与影像（`Without` / `With image`），引擎能将父节点的仿射变换等结果“冻结”并缓存，避免每帧重复计算复杂的树状变换开销。
*   **动态纹理图集 (Dynamic Atlas):** 内置基于二叉空间分割（BSP）的矩形打包算法，运行时自动将零碎小图合并为大尺寸 Texture Atlas，有效降低渲染时的纹理绑定开销。
*   **动静分离 UI 树:** 引擎在底层将 UI 组件拆分为交互层 (`Active`) 与纯视觉层 (`Inactive`)。静态组件不再参与鼠标事件的遍历与检测，显著减少了不必要的性能损耗。

---
