// Copyright 2020 Ben Manes. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package com.github.benmanes.caffeine.cache;

import java.lang.Object;
import java.lang.SuppressWarnings;
import java.lang.ref.ReferenceQueue;

/**
 * <em>WARNING: GENERATED CODE</em>
 *
 * A cache entry that provides the following features:
 * <ul>
 *   <li>MaximumWeight
 *   <li>WeakKeys (inherited)
 *   <li>WeakValues (inherited)
 *   <li>ExpireWrite (inherited)
 *   <li>RefreshWrite (inherited)
 * </ul>
 *
 * @author ben.manes@gmail.com (Ben Manes)
 */
@SuppressWarnings({"unchecked", "PMD.UnusedFormalParameter", "MissingOverride", "NullAway"})
final class FWWRMW<K, V> extends FWWR<K, V> {
  int queueType;

  int weight;

  int policyWeight;

  Node<K, V> previousInAccessOrder;

  Node<K, V> nextInAccessOrder;

  FWWRMW() {
  }

  FWWRMW(K key, ReferenceQueue<K> keyReferenceQueue, V value, ReferenceQueue<V> valueReferenceQueue,
      int weight, long now) {
    super(key, keyReferenceQueue, value, valueReferenceQueue, weight, now);
    this.weight = weight;
  }

  FWWRMW(Object keyReference, V value, ReferenceQueue<V> valueReferenceQueue, int weight,
      long now) {
    super(keyReference, value, valueReferenceQueue, weight, now);
    this.weight = weight;
  }

  public int getQueueType() {
    return queueType;
  }

  public void setQueueType(int queueType) {
    this.queueType = queueType;
  }

  public int getWeight() {
    return weight;
  }

  public void setWeight(int weight) {
    this.weight = weight;
  }

  public int getPolicyWeight() {
    return policyWeight;
  }

  public void setPolicyWeight(int policyWeight) {
    this.policyWeight = policyWeight;
  }

  public Node<K, V> getPreviousInAccessOrder() {
    return previousInAccessOrder;
  }

  public void setPreviousInAccessOrder(Node<K, V> previousInAccessOrder) {
    this.previousInAccessOrder = previousInAccessOrder;
  }

  public Node<K, V> getNextInAccessOrder() {
    return nextInAccessOrder;
  }

  public void setNextInAccessOrder(Node<K, V> nextInAccessOrder) {
    this.nextInAccessOrder = nextInAccessOrder;
  }

  public Node<K, V> newNode(K key, ReferenceQueue<K> keyReferenceQueue, V value,
      ReferenceQueue<V> valueReferenceQueue, int weight, long now) {
    return new FWWRMW<>(key, keyReferenceQueue, value, valueReferenceQueue, weight, now);
  }

  public Node<K, V> newNode(Object keyReference, V value, ReferenceQueue<V> valueReferenceQueue,
      int weight, long now) {
    return new FWWRMW<>(keyReference, value, valueReferenceQueue, weight, now);
  }
}
