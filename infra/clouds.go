package infra

import "fmt"

// The cloud emitters lower the agnostic resource graph to OpenTofu/Terraform HCL.
// The emitted HCL is consumed by EITHER `tofu` or `terraform` (the language is
// shared; only the CLI binary and provider registry differ), so one emitter per
// provider serves both IaC tools. Each emitter realizes EXACTLY the agnostic
// LogicalResource set — the cloud-specific scaffolding (resource groups, Service Bus
// namespaces, storage accounts, Pub/Sub subscriptions) is provider plumbing, not
// part of the logical set the equivalence gate compares.

// unsupported reports a resource kind a target does not lower. A clear error, never
// a panic.
func unsupported(target string, r Resource) error {
	return fmt.Errorf("infra: target %q does not yet support the %q resource %q",
		target, r.Kind(), r.LogicalName())
}

// hasKind reports whether the graph contains a resource of the given agnostic kind
// (drives which shared provider scaffolding to emit).
func hasKind(rs []Resource, kind string) bool {
	for _, r := range rs {
		if r.Kind() == kind {
			return true
		}
	}
	return false
}

// tfFile wraps an HCL string as the standard single-file artifact.
func tfFile(h *hcl, rs []Resource) Artifact {
	return Artifact{Files: map[string]string{"main.tf": h.String()}, Logical: logicalSet(rs)}
}

// AWS lowers to OpenTofu/Terraform HCL for Amazon Web Services.
type AWS struct{}

func (AWS) Target() string { return "aws" }
func (AWS) Cloud() bool    { return true }

func (AWS) Emit(rs []Resource) (Artifact, error) {
	h := &hcl{}
	h.open("terraform")
	h.open("required_providers")
	h.open("aws =")
	h.attr("source", str("hashicorp/aws"))
	h.close()
	h.close()
	h.close()
	h.blank()
	h.open("variable \"aws_region\"")
	h.attr("type", "string")
	h.attr("default", str("us-east-1"))
	h.close()
	h.blank()
	h.open("provider \"aws\"")
	h.attr("region", "var.aws_region")
	h.close()
	for _, r := range rs {
		h.blank()
		switch v := r.(type) {
		case Queue:
			h.open("resource \"aws_sqs_queue\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			if v.FIFO {
				h.attr("fifo_queue", "true")
			}
			h.close()
		case KV:
			h.open("resource \"aws_elasticache_cluster\" %s", str(v.Name))
			h.attr("cluster_id", str(v.Name))
			h.attr("engine", str("redis"))
			h.attr("node_type", str("cache.t3.micro"))
			h.attr("num_cache_nodes", "1")
			h.close()
		case Bucket:
			h.open("resource \"aws_s3_bucket\" %s", str(v.Name))
			h.attr("bucket", str(v.Name))
			h.close()
		default:
			return Artifact{}, unsupported("aws", r)
		}
	}
	return tfFile(h, rs), nil
}

// Azure lowers to OpenTofu/Terraform HCL for Microsoft Azure. Azure resources hang
// off a resource group; Service Bus needs a namespace, Blob needs a storage account
// (all shared plumbing, emitted once when a resource needs it).
type Azure struct{}

func (Azure) Target() string { return "azure" }
func (Azure) Cloud() bool    { return true }

func (Azure) Emit(rs []Resource) (Artifact, error) {
	h := &hcl{}
	h.open("terraform")
	h.open("required_providers")
	h.open("azurerm =")
	h.attr("source", str("hashicorp/azurerm"))
	h.close()
	h.close()
	h.close()
	h.blank()
	h.open("variable \"azure_location\"")
	h.attr("type", "string")
	h.attr("default", str("eastus"))
	h.close()
	h.blank()
	h.open("provider \"azurerm\"")
	h.emptyBlock("features")
	h.close()
	h.blank()
	h.open("resource \"azurerm_resource_group\" \"wavelet\"")
	h.attr("name", str("wavelet-rg"))
	h.attr("location", "var.azure_location")
	h.close()
	if hasKind(rs, "queue") {
		h.blank()
		h.open("resource \"azurerm_servicebus_namespace\" \"wavelet\"")
		h.attr("name", str("wavelet-ns"))
		h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
		h.attr("location", "azurerm_resource_group.wavelet.location")
		h.attr("sku", str("Standard"))
		h.close()
	}
	if hasKind(rs, "object") {
		h.blank()
		h.open("resource \"azurerm_storage_account\" \"wavelet\"")
		h.attr("name", str("waveletstorage"))
		h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
		h.attr("location", "azurerm_resource_group.wavelet.location")
		h.attr("account_tier", str("Standard"))
		h.attr("account_replication_type", str("LRS"))
		h.close()
	}
	for _, r := range rs {
		h.blank()
		switch v := r.(type) {
		case Queue:
			h.open("resource \"azurerm_servicebus_queue\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("namespace_id", "azurerm_servicebus_namespace.wavelet.id")
			if v.FIFO {
				h.attr("requires_session", "true")
			}
			h.close()
		case KV:
			h.open("resource \"azurerm_redis_cache\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
			h.attr("location", "azurerm_resource_group.wavelet.location")
			h.attr("capacity", "1")
			h.attr("family", str("C"))
			h.attr("sku_name", str("Basic"))
			h.close()
		case Bucket:
			h.open("resource \"azurerm_storage_container\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("storage_account_name", "azurerm_storage_account.wavelet.name")
			h.attr("container_access_type", str("private"))
			h.close()
		default:
			return Artifact{}, unsupported("azure", r)
		}
	}
	return tfFile(h, rs), nil
}

// GCP lowers to OpenTofu/Terraform HCL for Google Cloud.
type GCP struct{}

func (GCP) Target() string { return "gcp" }
func (GCP) Cloud() bool    { return true }

func (GCP) Emit(rs []Resource) (Artifact, error) {
	h := &hcl{}
	h.open("terraform")
	h.open("required_providers")
	h.open("google =")
	h.attr("source", str("hashicorp/google"))
	h.close()
	h.close()
	h.close()
	h.blank()
	h.open("variable \"gcp_project\"")
	h.attr("type", "string")
	h.close()
	h.blank()
	h.open("variable \"gcp_region\"")
	h.attr("type", "string")
	h.attr("default", str("us-central1"))
	h.close()
	h.blank()
	h.open("provider \"google\"")
	h.attr("project", "var.gcp_project")
	h.attr("region", "var.gcp_region")
	h.close()
	for _, r := range rs {
		h.blank()
		switch v := r.(type) {
		case Queue:
			h.open("resource \"google_pubsub_topic\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.close()
			h.blank()
			h.open("resource \"google_pubsub_subscription\" %s", str(v.Name+"_sub"))
			h.attr("name", str(v.Name+"-sub"))
			h.attr("topic", "google_pubsub_topic."+v.Name+".name")
			h.close()
		case KV:
			h.open("resource \"google_redis_instance\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("memory_size_gb", "1")
			h.close()
		case Bucket:
			h.open("resource \"google_storage_bucket\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("location", "var.gcp_region")
			h.close()
		default:
			return Artifact{}, unsupported("gcp", r)
		}
	}
	return tfFile(h, rs), nil
}
